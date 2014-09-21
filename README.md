# SON of JSON

A Scala library for dealing with JSON in a way that makes it almost feel native. 

① It requires just one import

```import nl.typeset.sonofjson._```

② Creating an object is easy

    // Parsing it
    val person = parse("""{ "name" : { "first" : "John", "last" : "Doe" } }""")
    
    // Building it
    val person = obj(
      name = obj(
         first = "John",
         last = "Doe"
      )
    )
    
③ Accessing it is easy too

	// Access the object and ask for a String representation
    person.name.first.as[String]
    
    // Or leave it to SON of JSON to get you a String representation
    var first: String = person.name.first
    
④ Modifying it is even easier

    person.name.first = "Jack"




