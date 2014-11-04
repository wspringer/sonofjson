[![Build status](https://travis-ci.org/wspringer/sonofjson.svg?branch=master)](https://travis-ci.org/wspringer/sonofjson)

# SON of JSON

A Scala library for dealing with JSON in a way that makes it almost feel native. If you want to understand how it compares to json4s, you might be interested to read about it [here](http://nxt.flotsam.nl/son-of-json-ii).


① It requires just one import

```import nl.typeset.sonofjson._```

② Creating an object is easy

    // You can parse it from a String
    val person = parse("""{ "name" : { "first" : "John", "last" : "Doe" } }""")
    
    // Or build it yourself
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

⑤ And rendering it to JSON is just

    render(person)


