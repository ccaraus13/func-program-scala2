(1 to 10).toList
(1 to 10).toList.foldLeft(2)(_ + _)
(1 to 10).toList.scanLeft(2)((b, a) => a + b)