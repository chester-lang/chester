package chester.tyck

import chester.syntax.accociativity.OperatorsContext
import chester.syntax.*
import chester.syntax.core.*
import chester.syntax.Name
import chester.tyck.BuiltIn.BuiltinItem
import chester.tyck.api.SymbolCollector
import chester.utils.propagator.*
import chester.uniqid.*

trait ProvideCtx extends ProvideCellId with ElaboraterBase {

  implicit class TyAndValOpsss(ignored: TyAndVal.type) {
    def create(ty: Term, value: Term)(using
        state: StateAbility[Tyck]
    ): TyAndVal = {
      new TyAndVal(toTerm(literal(ty)), toTerm(literal(value)))
    }

    def create()(using state: StateAbility[Tyck]): TyAndVal = {
      new TyAndVal(toTerm(state.addCell(OnceCell[Term]())), toTerm(state.addCell(OnceCell[Term]())))
    }
  }

  extension (context: ContextItem) {
    def tyId(using state: StateAbility[Tyck]): CellId[Term] = toId(context.ty)

    def tyTerm(using state: StateAbility[Tyck]): Term = toTerm(context.ty)
  }

  implicit class ContextItemObject(ignored: ContextItem.type) {
    def builtin(
        item: BuiltinItem
    )(using state: StateAbility[Tyck]): (TyAndVal, ContextItem) = {
      val varId = UniqId.generate[ToplevelV]
      val name = ToplevelV(AbsoluteRef(BuiltinModule, item.id), item.ty, varId)
      val ty1 = state.toId(item.ty)
      (
        new TyAndVal(toTerm(ty1), item.value),
        new ContextItem(item.id, varId, name, toTerm(ty1))
      )
    }
  }

  implicit class TyAndValOps(tyandval: TyAndVal) {
    def tyId(using state: StateAbility[Tyck]): CellId[Term] = toId(tyandval.ty)

    def valueId(using state: StateAbility[Tyck]): CellId[Term] = toId(tyandval.value)

    def tyTerm(using state: StateAbility[Tyck]): Term = toTerm(tyandval.ty)

    def valueTerm(using state: StateAbility[Tyck]): Term = toTerm(tyandval.value)

  }

  implicit class LocalCtxOps(ignored: Context.type) {
    def default(using state: StateAbility[Tyck]): Context = {
      val items = BuiltIn.builtinItems.map(ContextItem.builtin)
      val map = items.map(item => item._2.name -> item._2.uniqId).toMap
      val contextItems = items.map(item => item._2.uniqId -> item._2).toMap
      val knownMap: Map[UniqIdOf[? <: MaybeVarCall], TyAndVal] = items
        .map(item => item._2.uniqId -> item._1)
        .toMap
        .asInstanceOf[Map[UniqIdOf[? <: MaybeVarCall], TyAndVal]]
      new Context(map, contextItems, knownMap)
    }
  }

}

case class TyAndVal(
    ty: Term,
    value: Term
) {}

object TyAndVal {}

/** for pure values only like let and def. record is not included */
case class ContextItem(
    name: Name,
    uniqId: UniqIdOf[? <: MaybeVarCall],
    ref: MaybeVarCall,
    ty: Term,
    reference: Option[SymbolCollector] = None
)
object ContextItem {}
case class Imports()

object Imports {
  val Empty: Imports = Imports()
}

case class Context(
    map: Map[Name, UniqIdOf[? <: MaybeVarCall]] = Map.empty[Name, UniqIdOf[? <: MaybeVarCall]], // empty[...] are needed because compiler bugs
    contextItems: Map[UniqIdOf[? <: MaybeVarCall], ContextItem] =
      Map.empty[UniqIdOf[? <: MaybeVarCall], ContextItem], // empty[...] are needed because compiler bugs
    knownMap: Map[UniqIdOf[? <: MaybeVarCall], TyAndVal] =
      Map.empty[UniqIdOf[? <: MaybeVarCall], TyAndVal], // empty[...] are needed because compiler bugs
    typeDefinitionNames: Map[Name, UniqIdOf[TypeDefinition]] = Map.empty,
    typeDefinitions: Map[UniqIdOf[TypeDefinition], TypeDefinition] = Map.empty,
    imports: Imports = Imports.Empty,
    loadedModules: LoadedModules = LoadedModules.Empty,
    operators: OperatorsContext = OperatorsContext.Default,
    currentModule: ModuleRef = DefaultModule
) {
  def updateModule(module: ModuleRef): Context = copy(currentModule = module)

  def getKnown(x: MaybeVarCall): Option[TyAndVal] =
    knownMap.get(x.uniqId.asInstanceOf[UniqIdOf[? <: MaybeVarCall]])

  def get(id: Name): Option[ContextItem] =
    map.get(id).flatMap(uniqId => contextItems.get(uniqId))

  def knownAdd(id: UniqIdOf[? <: MaybeVarCall], y: TyAndVal): Context =
    knownAdd(Seq(id -> y))

  def knownAdd(
      seq: Seq[(UniqIdOf[? <: MaybeVarCall], TyAndVal)]
  ): Context = {
    val newKnownMap = seq.foldLeft(knownMap) { (acc, item) =>
      assert(!acc.contains(item._1), s"Duplicate key ${item._1}")
      acc + item
    }
    copy(knownMap = newKnownMap)
  }

  def add(item: ContextItem): Context = add(Seq(item))

  def add(seq: Seq[ContextItem]): Context = {
    val newMap = seq.foldLeft(map) { (acc, item) =>
      acc + (item.name -> item.uniqId)
    }
    val newContextItems = seq.foldLeft(contextItems) { (acc, item) =>
      require(!acc.contains(item.uniqId), s"Duplicate key ${item.uniqId}")
      acc + (item.uniqId -> item)
    }
    copy(map = newMap, contextItems = newContextItems)
  }
  def addTypeDefinition(typeDef: TypeDefinition): Context = {
    copy(
      typeDefinitionNames = typeDefinitionNames + (typeDef.name -> typeDef.uniqId),
      typeDefinitions = typeDefinitions + (typeDef.uniqId -> typeDef)
    )
  }

  def getTypeDefinition(name: Name): Option[TypeDefinition] = {
    typeDefinitionNames.get(name).flatMap(typeDefinitions.get)
  }

  def getTypeDefinitionById(id: UniqIdOf[TypeDefinition]): Option[TypeDefinition] = {
    typeDefinitions.get(id)
  }
}

object Context {}
