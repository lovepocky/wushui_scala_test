import Problem201608._
import org.specs2.Specification
import org.specs2.specification.core.SpecStructure


/**
  * Created by lovepocky on 16/8/27.
  */
object Test201608 extends Specification {
    override def is: SpecStructure =
        s2"""
           Test Of 201608 should
            sample  $sample
            nest    $nest
            multi   $multi
        """


    val sampleDefs: Definitions = Seq("User" -> Seq("name" -> "oldpig", "gender" -> "male", "mixin" -> "Address"),
        "Address" -> Seq("Province" -> "Shanghai", "District" -> "Minhang"))
    val sampleResult: Definitions = Seq("User" -> Seq("name" -> "oldpig", "gender" -> "male", "Province" -> "Shanghai", "District" -> "Minhang"),
        "Address" -> Seq("Province" -> "Shanghai", "District" -> "Minhang"))

    def sample = expand(sampleDefs) must_== sampleResult

    val nestDefs: Definitions =
        Seq("User" -> Seq("name" -> "oldpig", "gender" -> "male", "mixin" -> "Address"),
            "Address" -> Seq("Province" -> "Shanghai", "District" -> "Minhang", "mixin" -> "B"),
            "B" -> Seq("mixin" -> "C", "fieldB1" -> "valueB1"),
            "C" -> Seq("fieldC1" -> "valueC1"))
    val nestResult: Definitions =
        Seq("User" -> Seq("name" -> "oldpig", "gender" -> "male", "Province" -> "Shanghai", "District" -> "Minhang", "fieldB1" -> "valueB1", "fieldC1" -> "valueC1"),
            "Address" -> Seq("Province" -> "Shanghai", "District" -> "Minhang", "fieldB1" -> "valueB1", "fieldC1" -> "valueC1"),
            "B" -> Seq("fieldB1" -> "valueB1", "fieldC1" -> "valueC1"),
            "C" -> Seq("fieldC1" -> "valueC1"))

    def nest = expand(nestDefs) must_== nestResult

    val multiDefs: Definitions =
        Seq("User" -> Seq("name" -> "oldpig", "gender" -> "male", "mixin" -> "Address"),
            "Address" -> Seq("Province" -> "Shanghai", "District" -> "Minhang", "mixin" -> "C"),
            "B" -> Seq("mixin" -> "C", "fieldB1" -> "valueB1"),
            "C" -> Seq("fieldC1" -> "valueC1"))
    val multiResult: Definitions =
        Seq("User" -> Seq("name" -> "oldpig", "gender" -> "male", "Province" -> "Shanghai", "District" -> "Minhang", "fieldC1" -> "valueC1"),
            "Address" -> Seq("Province" -> "Shanghai", "District" -> "Minhang", "fieldC1" -> "valueC1"),
            "B" -> Seq("fieldB1" -> "valueB1", "fieldC1" -> "valueC1"),
            "C" -> Seq("fieldC1" -> "valueC1"))

    def multi = expand(multiDefs) must_== multiResult
}
