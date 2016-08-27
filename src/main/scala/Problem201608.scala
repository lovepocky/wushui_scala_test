/**
  * Created by lovepocky on 16/8/27.
  */
object Problem201608 {

    type Definitions = Seq[(String, Seq[(String, String)])]

    def main(args: Array[String]) {
        val defs: Definitions = Seq("User" -> Seq("name" -> "oldpig", "gender" -> "male", "mixin" -> "Address"),
            "Address" -> Seq("Province" -> "Shanghai", "District" -> "Minhang"))

        assert(expand(defs) == Seq("User" -> Seq("name" -> "oldpig", "gender" -> "male", "Province" -> "Shanghai", "District" -> "Minhang"),
            "Address" -> Seq("Province" -> "Shanghai", "District" -> "Minhang")))

    }

    def expand(d: Definitions): Definitions = {
        val dictionary = d.toMap[String, Seq[(String, String)]]

        //替换mixin时添加到最后
        def expandOne(one: (String, Seq[(String, String)])): (String, Seq[(String, String)]) = {
            one._2.exists(_._1 == "mixin") match {
                case false => one
                case true =>
                    expandOne(
                        one.copy(
                            _2 = one._2.filterNot(_._1 == "mixin") ++
                                one._2.filter(_._1 == "mixin").flatMap(it => dictionary.getOrElse(it._2, Seq()))
                        )
                    )
            }
        }
        d.map(expandOne)
    }
}
