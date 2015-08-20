package com.yarenty.visualize


import scalax.chart.api._

/**
 * @author yarenty
 */
object DrawChart {
  
  
  def main(args: Array[String]): Unit = {
  
    val data = for (i <- 1 to 5 ) yield (i,i)
    
    val initial = List((0,0.5),(1,0.66),(5,0.83),(8,0.34),(10,0.22))
    val normalized = List((0,0.5),(1,0.66),(2,0.0),(3,0.0),(4,0.0),(5,0.83),(6,0.0),(7,0.0),(8,0.34),(9,0.0),(10,0.22))
    val interpolated = List((0,0.5),(1,0.52),(2,0.54),(3,0.56),(4,0.58000004),(5,0.6),(6,0.62),(7,0.64000005),(8,0.66),(9,0.57750005),(10,0.495),(11,0.41250002),(12,0.33),(13,0.2475),(14,0.165),(15,0.0825),(16,0.0),(17,0.0),(18,0.0),(19,0.0),(20,0.0),(21,0.0),(22,0.0),(23,0.0),(24,0.0),(25,0.0),(26,0.0),(27,0.0),(28,0.0),(29,0.0),(30,0.0),(31,0.0),(32,0.0),(33,0.10375),(34,0.2075),(35,0.31125),(36,0.415),(37,0.51875),(38,0.6225),(39,0.72625),(40,0.83),(41,0.72625),(42,0.6225),(43,0.51875),(44,0.415),(45,0.31125),(46,0.2075),(47,0.10375),(48,0.0),(49,0.0),(50,0.0),(51,0.0),(52,0.0),(53,0.0),(54,0.0),(55,0.0),(56,0.0),(57,0.0425),(58,0.085),(59,0.1275),(60,0.17),(61,0.2125),(62,0.255),(63,0.2975),(64,0.34),(65,0.2975),(66,0.255),(67,0.2125),(68,0.17),(69,0.1275),(70,0.085),(71,0.0425),(72,0.0),(73,0.0275),(74,0.055),(75,0.082499996),(76,0.11),(77,0.1375),(78,0.16499999),(79,0.1925),(80,0.22),(81,0.1925),(82,0.16499999),(83,0.1375),(84,0.11),(85,0.082499996),(86,0.055),(87,0.0275));
    val blured =  List((0,0.7035),(1,0.71849996),(2,0.7455),(3,0.77400005),(4,0.8020001),(5,0.8300001),(6,0.85800004),(7,0.8834376),(8,0.9079376),(9,0.91031253),(10,0.8306875),(11,0.71775),(12,0.60225004),(13,0.48675004),(14,0.37125),(15,0.25368753),(16,0.12581252),(17,0.030937502),(18,0.0020625002),(19,0.0),(20,0.0),(21,0.0),(22,0.0),(23,0.0),(24,0.0),(25,0.0),(26,0.0),(27,0.0),(28,0.0),(29,0.0),(30,0.0),(31,0.00259375),(32,0.01815625),(33,0.095968746),(34,0.23603125),(35,0.383875),(36,0.529125),(37,0.67437506),(38,0.81962496),(39,0.9596875),(40,1.0738126),(41,1.1153126),(42,1.0426875),(43,0.90262496),(44,0.75737506),(45,0.61212504),(46,0.46687502),(47,0.31903127),(48,0.15821874),(49,0.03890625),(50,0.00259375),(51,0.0),(52,0.0),(53,0.0),(54,0.0),(55,0.0010625),(56,0.0074375),(57,0.0393125),(58,0.096687496),(59,0.15725),(60,0.21675001),(61,0.27625),(62,0.33575),(63,0.39312503),(64,0.43987504),(65,0.45687503),(66,0.42712504),(67,0.36975),(68,0.31025),(69,0.25075),(70,0.19125001),(71,0.13),(72,0.06),(73,0.026500002),(74,0.0615),(75,0.10175001),(76,0.14025001),(77,0.17875001),(78,0.21725),(79,0.25503904),(80,0.2892734),(81,0.30644533),(82,0.3073047),(83,0.30565625),(84,0.30434376),(85,0.30303127),(86,0.30169532),(87,0.3002422))
    
    val chartNormalized = XYBarChart(normalized, "just normalized")
    val chartInterpolated = XYBarChart(interpolated,"interpolated")
    val chartBlurred = XYBarChart(blured, "blured")
    val chartLineBlurred = XYLineChart(blured, "blured")
    
    chartNormalized.show("Interpolation - normalized", (1000,600), true)
    chartInterpolated.show("Interpolation - after interpolation", (1000,600), true)
    chartBlurred.show("Interpolation - after blur filter", (1000,600), true)
    chartLineBlurred.show("Interpolation Line blur version", (1000,600), true)
    
  }
  
}