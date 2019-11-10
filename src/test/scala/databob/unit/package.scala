package databob

package object unit {

  // custom types for testing
  type Longer = Long
  type LongerList = List[Longer]

  class CustomVal(val a: Int) extends AnyVal
}
