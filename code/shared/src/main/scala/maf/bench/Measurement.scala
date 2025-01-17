package maf.bench

import scala.collection.mutable.ListBuffer

class Measurement(groupBy: Int = 1, warmup: Int, strategy: String, file: String):
    var mean: Option[Double] = None
    var sd: Option[Double] = None
    var n: Option[Double] = None
    var min: Option[Double] = None
    var max: Option[Double] = None
    var median: Option[Double] = None

    var measurements: ListBuffer[Double] = ListBuffer()

    def calculate(): Unit =
        val noWarmupmMasurements = measurements.slice(warmup, measurements.length)

        val groupedMeasurements: List[Double] = noWarmupmMasurements.grouped(groupBy).toList.map(_.sum)
        measurements = groupedMeasurements.to(ListBuffer)
        mean = Some(computeAvg)
        sd = Some(computeSd)
        n = Some(measurements.length.toDouble)
        val sortedMeasurements = measurements.sorted
        min =  Some(sortedMeasurements.head)
        median = Some(sortedMeasurements(sortedMeasurements.length / 2))
        max = Some(sortedMeasurements.last)

    def addMeasurement(i: Double): Unit = measurements += i

    override def toString: String =
        s"$file,${mean.get},${1.96 * standardError},${min.get},${median.get},${max.get}"

    private def standardError: Double =
        sd.get / Math.sqrt(n.get)
    private def computeAvg =
        val sum = measurements.foldLeft(0.0)(_ + _)
        sum / measurements.length
    private def computeSd =
        val mean = computeAvg
        var sum = 0.0
        for(rt <- measurements) do
            sum += (rt - mean) * (rt - mean)

        Math.sqrt(sum / measurements.length)