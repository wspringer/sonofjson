package nl.typeset.sonofjson.model

import org.specs2.mutable.Specification

class ParserSpec extends Specification {

  "The parser" should {

    "be able to parse an object" in {
      val json = """
        |{ "person" : { "first" : "John", "last": "Doe" } }
      """.stripMargin.trim
      parse(json) must beLike {
        case JObject(obj) =>
          obj.get("person") must beSome
      }
    }

    "do something sensible with parsing errors" in {
      parse("{ [ 0, 1, 2 ]}") must throwA[ParserException].like {
        case ParserException(msg, pos) =>
          pos.line must be equalTo(1)
          pos.column must be equalTo(3)
      }
    }

    "do something sensible when there's not enough input" in {
      parse("{ ") must throwA[ParserException].like {
        case ParserException(msg, pos) =>
          pos.line must be equalTo(1)
          pos.column must be equalTo(3)
      }
    }

  }

}
