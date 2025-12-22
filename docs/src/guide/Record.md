# Record Syntax in Chester

Chester provides a concise and powerful syntax for defining records, which are similar to structs or classes in other languages. Records in Chester are immutable by default and provide a convenient way to group related data.

## Basic Record Syntax

The basic syntax for defining a record in Chester is as follows:

```chester
record RecordName(field1: Type1, field2: Type2, ...) { ... }
```

Here's a simple example of a `Person` record:

```chester,playground,editable
record Person(name: String, age: Int);
```

## Using Records

Once defined, you can create instances of records and access their fields:

```chester,playground,editable
let alice = Person("Alice", 30);
println(alice.name);  // Outputs: Alice
println(alice.age);   // Outputs: 30
```
