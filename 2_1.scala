object ReduceMap{
	def main(args: Array[String]) = {
		var A = List((1, List(2,3)), (3, List(1, 5)), (2, List(5)), (5, List()))
		println(A)
		println(A.sliding(1).toList)

// ========================================Mapping======================================================
		var Mapping = collection.mutable.ListBuffer[collection.mutable.ListBuffer[Int]]()
		for (word <- A){
			var pom = word._2.sliding(1).toList
			for (word1 <- pom){
				Mapping += collection.mutable.ListBuffer(word._1, word1(0))
			}
		}
		println(Mapping)

// ===============================================Reduce=================================================

		var Reduce = collection.mutable.ListBuffer[collection.mutable.ListBuffer[Int]]()
		for (word <- Mapping){
			Reduce += collection.mutable.ListBuffer(word(1), word(0))
		}
		println(Reduce)
		
// =============================================Combining================================================

//		var B = List[(Int, List[Int])]()
		var B = collection.mutable.Map[Int, collection.mutable.ListBuffer[Int]]()
		var pom1 = collection.mutable.ListBuffer[Int]()
		
		for (word <- Reduce){
			for (word1 <- Reduce){
				if (word(0) == word1(0)){
					pom1 += word1(1)
				}
			}
			println(word(0) + " -> " + pom1)
//			println(pom1)
			B += (word(0) -> pom1)
			pom1 = collection.mutable.ListBuffer()
			
			
		}

		print(B)
//		def Mapping(A: List[(Int, List[Int]]):List[Int] =collection.mutable.ListBuffer{
//			var Mapping = collection.mutable.ListBuffer[collection.mutable.ListBuffer[Int]]()
//			for (word <- A){
//				var pom = word._2.sliding(1).toList
//				for (word1 <- pom){
//					Mapping += collection.mutable.ListBuffer(word._1, word1(0))
//				}
//			}
//			return Mapping
//		}


	}
}
