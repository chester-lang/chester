package chester.elab

import chester.elab.LoadedModules
import chester.syntax.*
import chester.syntax.accociativity.OperatorsContext
import chester.syntax.core.*
import chester.elab.api.SymbolCollector
import chester.uniqid.*
import chester.reduce.ReduceContext
import chester.reduce.{DefaultReducer, Reducer}
import chester.i18n.*
import chester.elab.PreludeBuiltin.BuiltinItem

import scala.collection.immutable.HashMap

case class TyAndVal(
    ty: Term,
    value: Term
) {}

/** for pure values only like let and def. record is not included */
case class ContextItem(
    name: Name,
    uniqId: UniqidOf[Reference],
    ref: Reference,
    ty: Term,
    reference: Option[SymbolCollector] = None
)

object ContextItem {
  def builtin(
      item: BuiltinItem
  ): (TyAndVal, ContextItem) = {
    val varId = Uniqid.make[ToplevelVar]
    val name = ToplevelVar(AbsoluteRef(BuiltinModule, item.id), item.ty, varId, None)
    val ty1 = item.ty
    (
      TyAndVal(ty1, item.value),
      ContextItem(item.id, varId, name, ty1)
    )
  }
}

case class Imports()

object Imports {
  val Empty: Imports = Imports()
}

case class Features(preview: Boolean = false)
object Features {
  val Default: Features = Features()
}

object Context {

  val default: Context = {
    val items = PreludeBuiltin.builtinItems.map(ContextItem.builtin)
    val map = items.map(item => item._2.name -> item._2.uniqId).toMap
    val contextItems = items.map(item => item._2.uniqId -> item._2).toMap
    val knownMap: Map[UniqidOf[Reference], TyAndVal] = items
      .map(item => item._2.uniqId -> item._1)
      .toMap
      .asInstanceOf[Map[UniqidOf[Reference], TyAndVal]]
    Context(map = map, contextItems = contextItems, knownMap = knownMap)
  }
}

case class Def(uniqId: UniqidOf[Def]) extends HasUniqid {

  override def collectU(collector: UCollector): Unit =
    collector(uniqId)

  override def replaceU(reranger: UReplacer): Any = copy(
    uniqId = reranger(uniqId)
  )
}

case class ExtensionDefinition(
    uniqId: UniqidOf[ExtensionDefinition],
    ref: AbsoluteRef,
    ty: Term,
    bind: LocalVar,
    methods: Map[Name, Def] = HashMap.empty
) extends HasUniqid {

  override def collectU(collector: UCollector): Unit = {
    collector(uniqId)
    ty.collectU(collector)
    bind.collectU(collector)
    methods.values.foreach(_.collectU(collector))
  }

  override def replaceU(reranger: UReplacer): Any =
    copy(
      uniqId = reranger(uniqId),
      ty = ty.replaceU(reranger),
      bind = bind.replaceU(reranger).asInstanceOf[LocalVar],
      methods = methods.map { case (name, defn) =>
        name -> defn.replaceU(reranger).asInstanceOf[Def]
      }
    )
}

case class Context(
    effects: EffectsM = Effects.Empty,
    features: Features = Features.Default,
    map: Map[Name, UniqidOf[Reference]] = HashMap.empty[Name, UniqidOf[Reference]], // empty[...] are needed because compiler bugs
    contextItems: Map[UniqidOf[Reference], ContextItem] =
      HashMap.empty[UniqidOf[Reference], ContextItem], // empty[...] are needed because compiler bugs
    knownMap: Map[UniqidOf[Reference], TyAndVal] = HashMap.empty[UniqidOf[Reference], TyAndVal], // empty[...] are needed because compiler bugs
    typeDefinitionNames: Map[Name, UniqidOf[TypeDefinition]] = HashMap.empty,
    typeDefinitions: Map[UniqidOf[TypeDefinition], TypeDefinition] = HashMap.empty,
    // all extensions
    extensions: Map[UniqidOf[ExtensionDefinition], ExtensionDefinition] = HashMap.empty,
    // activated extensions
    extensionsLookup: Vector[UniqidOf[ExtensionDefinition]] = Vector.empty,
    imports: Imports = Imports.Empty,
    loadedModules: LoadedModules = LoadedModules.Empty,
    operators: OperatorsContext = OperatorsContext.Default,
    currentModule: ModuleRef = DefaultModule
) {
  def updateModule(module: ModuleRef): Context = copy(currentModule = module)

  def getKnown(x: Reference): Option[TyAndVal] =
    knownMap.get(x.uniqId.asInstanceOf[UniqidOf[Reference]])

  def get(id: Name): Option[ContextItem] =
    map.get(id).flatMap(contextItems.get)

  def knownAdd(id: UniqidOf[Reference], y: TyAndVal): Context =
    knownAdd(Seq(id -> y))

  def knownAdd(
      seq: Seq[(UniqidOf[Reference], TyAndVal)]
  ): Context = {
    val newKnownMap = seq.foldLeft(knownMap) { (acc, item) =>
      assert(!acc.contains(item._1), t"Duplicate key ${item._1}")
      acc + item
    }
    copy(knownMap = newKnownMap)
  }

  def add(item: ContextItem): Context = add(Seq(item))

  def add(seq: Seq[ContextItem]): Context = {
    val newMap = seq.foldLeft(map)((acc, item) => acc + (item.name -> item.uniqId))
    val newContextItems = seq.foldLeft(contextItems) { (acc, item) =>
      require(!acc.contains(item.uniqId), t"Duplicate key ${item.uniqId}")
      acc + (item.uniqId -> item)
    }
    copy(map = newMap, contextItems = newContextItems)
  }
  def addTypeDefinition(typeDef: TypeDefinition): Context =
    copy(
      typeDefinitionNames = typeDefinitionNames ++ typeDef.names.map(_ -> typeDef.uniqId).iterator,
      typeDefinitions = typeDefinitions + (typeDef.uniqId -> typeDef)
    )

  def getTypeDefinition(name: Name): Option[TypeDefinition] = {
    val uniqId = typeDefinitionNames.get(name)
    if (uniqId.isEmpty) return None
    val r = typeDefinitions.get(uniqId.get)
    r
  }

  def getTypeDefinitionById(id: UniqidOf[TypeDefinition]): Option[TypeDefinition] =
    typeDefinitions.get(id)

  def toReduceContext: ReduceContext = ReduceContext() // TODO: Implement proper state handling

  given Reducer = DefaultReducer
}
