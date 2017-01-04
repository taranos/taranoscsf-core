//
// Taranos Cloud Sonification Framework: Service Core
// Copyright (C) 2017 David Hinson, Netrogen Blue LLC (dhinson@netrogenblue.com)
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

package org.taranos.mc.field

import org.taranos.mc.Cell
import org.taranos.mc.Common.{Real, ReportSectionsParser, Reportable}
import org.taranos.mc.trunk.intraprocess.Signal
import org.taranos.mc.trunk.intraprocess.Signal.{Continuous, Discrete}
import play.api.libs.json._


/*
Notes:

// Schema:
{
    // envelope definition (continuous):
    de:
    {
        c: "",      // ceiling magnitude (default = 1.0)
        f: "",      // floor magnitude (default = 0.0)
        dp: [],     // pole definitions (either dp or dpp REQUIRED; may be empty)
        dpp: ""     // packed pole definitions (either dp or dpp REQUIRED; may be empty)
    }

    // envelope definition (discrete):
    de:
    {
        dp: [],     // pole definitions (either dp or dpc REQUIRED; may be empty)
        dpp: ""     // packed pole definitions (either dp or dpc REQUIRED; may be empty)
    }

    // pole definition:
    dp:
    {
        x: "",      // pole-x (REQUIRED; 0-15)
        y: ""       // pole-y (REQUIRED; 0-15)
    }

    // Unity gain envelope definition (continuous):
    {
        dp: [
            {x: "min", y: "min"},
            {x: "max", y: "max"}]
    }

    // Inverse unity gain envelope definition (continuous):
    {
        dp: [
            {x: "min", y: "max"},
            {x: "max", y: "min"}]
    }

    // Max gain envelope definition (continuous):
    {
        dp: [
            {x: "min", y: "max"},
            {x: "max", y: "max"}]
    }

    // Two-thirds gain with dead zone envelope definition (continuous):
    {
        dp: [
            {x: "min", y: "min"},
            {x: "1", y: "10"},
            {x: "max", y: "10"}]
    }

    // Min gain envelope definition (continuous):
    {
        dp: [
            {x: "min", y: "min"},
            {x: "max", y: "min"}]
    }

    // Zero envelope definition (discrete):
    {
        dp: [
            {x: "min", y: "min"}]
    }
}


// Packed poles binary format:

Pole    X   Y
----    --  --
0       0   n0
1       n1  n2
2       n3  n4
3       n5  n6
4       F   n7

= 0x{n7}{n6}{n5}{n4}{n3}{n2}{n1}{n0}

*/


object Envelope
{
    import scala.collection.mutable

    val kUnityGainEnvelopePolesDef = Json.arr(
        Json.obj(
            FieldModel.Glossary.kEnvelopePolesDefX -> FieldModel.Glossary.kEnvelopePolesDefMin,
            FieldModel.Glossary.kEnvelopePolesDefY -> FieldModel.Glossary.kEnvelopePolesDefMin),
        Json.obj(
            FieldModel.Glossary.kEnvelopePolesDefX -> FieldModel.Glossary.kEnvelopePolesDefMax,
            FieldModel.Glossary.kEnvelopePolesDefY -> FieldModel.Glossary.kEnvelopePolesDefMax))
    val kUnityGainEnvelopeDef = Json.obj(
        FieldModel.Glossary.kEnvelopePolesDef -> kUnityGainEnvelopePolesDef)

    val kInverseUnityGainEnvelopePolesDef = Json.arr(
        Json.obj(
            FieldModel.Glossary.kEnvelopePolesDefX -> FieldModel.Glossary.kEnvelopePolesDefMin,
            FieldModel.Glossary.kEnvelopePolesDefY -> FieldModel.Glossary.kEnvelopePolesDefMax),
        Json.obj(
            FieldModel.Glossary.kEnvelopePolesDefX -> FieldModel.Glossary.kEnvelopePolesDefMax,
            FieldModel.Glossary.kEnvelopePolesDefY -> FieldModel.Glossary.kEnvelopePolesDefMin))
    val kInverseUnityGainEnvelopeDef = Json.obj(
        FieldModel.Glossary.kEnvelopePolesDef -> kInverseUnityGainEnvelopePolesDef)

    val kMaxGainEnvelopePolesDef = Json.arr(
        Json.obj(
            FieldModel.Glossary.kEnvelopePolesDefX -> FieldModel.Glossary.kEnvelopePolesDefMin,
            FieldModel.Glossary.kEnvelopePolesDefY -> FieldModel.Glossary.kEnvelopePolesDefMax),
        Json.obj(
            FieldModel.Glossary.kEnvelopePolesDefX -> FieldModel.Glossary.kEnvelopePolesDefMax,
            FieldModel.Glossary.kEnvelopePolesDefY -> FieldModel.Glossary.kEnvelopePolesDefMax))
    val kMaxGainEnvelopeDef = Json.obj(
        FieldModel.Glossary.kEnvelopePolesDef -> kMaxGainEnvelopePolesDef)

    val kTwoThirdsGainWithDeadZoneEnvelopePolesDef = Json.arr(
        Json.obj(
            FieldModel.Glossary.kEnvelopePolesDefX -> FieldModel.Glossary.kEnvelopePolesDefMin,
            FieldModel.Glossary.kEnvelopePolesDefY -> FieldModel.Glossary.kEnvelopePolesDefMin),
        Json.obj(
            FieldModel.Glossary.kEnvelopePolesDefX -> 1.toString,
            FieldModel.Glossary.kEnvelopePolesDefY -> 10.toString),
        Json.obj(
            FieldModel.Glossary.kEnvelopePolesDefX -> FieldModel.Glossary.kEnvelopePolesDefMax,
            FieldModel.Glossary.kEnvelopePolesDefY -> 10.toString))
    val kTwoThirdsGainWithDeadZoneEnvelopeDef = Json.obj(
        FieldModel.Glossary.kEnvelopePolesDef -> kTwoThirdsGainWithDeadZoneEnvelopePolesDef)

    val kMinGainEnvelopePolesDef = Json.arr(
        Json.obj(
            FieldModel.Glossary.kEnvelopePolesDefX -> FieldModel.Glossary.kEnvelopePolesDefMin,
            FieldModel.Glossary.kEnvelopePolesDefY -> FieldModel.Glossary.kEnvelopePolesDefMin),
        Json.obj(
            FieldModel.Glossary.kEnvelopePolesDefX -> FieldModel.Glossary.kEnvelopePolesDefMax,
            FieldModel.Glossary.kEnvelopePolesDefY -> FieldModel.Glossary.kEnvelopePolesDefMin))
    val kMinGainEnvelopeDef = Json.obj(
        FieldModel.Glossary.kEnvelopePolesDef -> kMinGainEnvelopePolesDef)

    val kZeroEnvelopePolesDef = Json.arr(
        Json.obj(
            FieldModel.Glossary.kEnvelopePolesDefX -> FieldModel.Glossary.kEnvelopePolesDefMin,
            FieldModel.Glossary.kEnvelopePolesDefY -> FieldModel.Glossary.kEnvelopePolesDefMin))
    val kZeroEnvelopeDef = Json.obj(
        FieldModel.Glossary.kEnvelopePolesDef -> kZeroEnvelopePolesDef)

    class ContinuousEnvelope (
        var _ceiling: Continuous = 1f,
        var _floor: Continuous = 0f,
        val _segments: mutable.ArrayBuffer[(Continuous, Continuous, Continuous)] =
        mutable.ArrayBuffer.empty[(Continuous, Continuous, Continuous)])
        extends Reportable
    {
        override
        def Report (sections: ReportSectionsParser): JsObject =
        {
            val segments = _segments.map(segment =>
            {
                val (x, y, s) = segment
                Json.obj(
                    FieldModel.Glossary.kEnvelopeSegmentX -> Reportable.ReportReal3(x),
                    FieldModel.Glossary.kEnvelopeSegmentY -> Reportable.ReportReal3(y),
                    FieldModel.Glossary.kEnvelopeSegmentSlope -> Reportable.ReportReal3(s))
            })
            val report =
                Json.obj(
                    FieldModel.Glossary.kEnvelopeCeiling -> Reportable.ReportReal3(_ceiling),
                    FieldModel.Glossary.kEnvelopeFloor -> Reportable.ReportReal3(_floor),
                    FieldModel.Glossary.kEnvelopeSegment -> segments)

            report
        }
    }

    class DiscreteEnvelope (
        val _segments: mutable.ArrayBuffer[(Continuous, Discrete)] =
        mutable.ArrayBuffer.empty[(Continuous, Discrete)])
        extends Reportable
    {
        override
        def Report (sections: ReportSectionsParser): JsObject =
        {
            val segments = _segments.map(segment =>
            {
                val (x, y) = segment
                Json.obj(
                    FieldModel.Glossary.kEnvelopeSegmentX -> Reportable.ReportReal3(x),
                    FieldModel.Glossary.kEnvelopeSegmentY -> Reportable.ReportInteger(y))
            })
            val report =
                Json.obj(
                    FieldModel.Glossary.kEnvelopeSegment -> segments)

            report
        }
    }

    private
    def ExtractPolesDef (polesDef: Vector[JsObject]): Vector[(Int, Int)] =
    {
        polesDef.map(poleDef =>
        {
            val x: Int =
                (poleDef \ FieldModel.Glossary.kEnvelopePolesDefX).validate[String] match
                {
                    case JsSuccess(validated, _) =>
                        validated match
                        {
                            case FieldModel.Glossary.kEnvelopePolesDefMin => 0
                            case FieldModel.Glossary.kEnvelopePolesDefMax => 15
                            case _ =>
                                val value = validated.toInt
                                if (value < 0 || value > 15)
                                    throw new FieldException(Cell.ErrorCodes.EnvelopeDefinitionInvalid)
                                value
                        }

                    case JsError(errors) =>
                        throw new FieldException(Cell.ErrorCodes.EnvelopeDefinitionInvalid)
                }

            val y: Int =
                (poleDef \ FieldModel.Glossary.kEnvelopePolesDefY).validate[String] match
                {
                    case JsSuccess(validated, _) =>
                        validated match
                        {
                            case FieldModel.Glossary.kEnvelopePolesDefMin => 0
                            case FieldModel.Glossary.kEnvelopePolesDefMax => 15
                            case _ =>
                                val value = validated.toInt
                                if (value < 0 || value > 15)
                                    throw new FieldException(Cell.ErrorCodes.EnvelopeDefinitionInvalid)
                                value
                        }

                    case JsError(errors) =>
                        throw new FieldException(Cell.ErrorCodes.EnvelopeDefinitionInvalid)
                }

            (x, y)
        })
    }

    private
    def UnpackPoles (polesPacked: String): Vector[(Int, Int)] =
    {
        def IsHex (text: String): Boolean =
        {
            val kHexDigits = "0123456789ABCDEFabcdef"
            text.forall(kHexDigits.contains(_))
        }

        // Test validity:
        if (polesPacked.isEmpty || polesPacked.length > 8 || !IsHex(polesPacked))
            throw new FieldException(Cell.ErrorCodes.PoleDefinitionInvalid)

        // Parse reverse of packed poles and yield a vector of JsObjects:
        var longPoles = java.lang.Long.parseLong(polesPacked.reverse, 16)
        // Beware: maxCount value is tricky since it relies on integer rounding-down to work.
        val maxCount = polesPacked.length / 2
        (for (count <- 0 to maxCount) yield
        {
            count match
            {
                case 0 | 4 =>
                    val tuple = (if (count == 0) 0 else 15, (longPoles & 0xf).toInt)
                    longPoles = longPoles >> 4
                    tuple

                case _ =>
                    val tuple = ((longPoles & 0xf).toInt, ((longPoles & 0xf0) >> 4).toInt)
                    longPoles = longPoles >> 8
                    tuple
            }
        }).toVector
    }

    def DecodeContinuousEnvelopeDef (envelopeDef: JsObject): Envelope.ContinuousEnvelope =
    {
        val newEnvelope = new Envelope.ContinuousEnvelope()

        // Determine new ceiling:
        val newCeiling: Signal.Continuous = (envelopeDef \ FieldModel.Glossary.kEnvelopeCeiling).validate[String] match
        {
            case JsSuccess(value, _) =>
                val proposedCeiling = value.toFloat
                // Only accept proposed value if within valid range, otherwise substitute default value:
                if (proposedCeiling > 0f && proposedCeiling <= 1f)
                    proposedCeiling
                else
                    newEnvelope._ceiling

            case JsError(errors) => newEnvelope._ceiling
        }

        // Determine new floor:
        val newFloor: Signal.Continuous = (envelopeDef \ FieldModel.Glossary.kEnvelopeFloor).validate[String] match
        {
            case JsSuccess(value, _) =>
                val proposedFloor = value.toFloat
                // Only accept proposed value if within valid range, otherwise substitute default value:
                if (proposedFloor >= 0f && proposedFloor < 1f)
                    proposedFloor
                else
                    newEnvelope._floor

            case JsError(errors) => newEnvelope._floor
        }

        // Override default ceiling and floor values with new ones only if they are reasonable:
        if (newFloor < newCeiling)
        {
            newEnvelope._ceiling = newCeiling
            newEnvelope._floor = newFloor
        }

        // Extract pole definitions:
        val extractedPoleDefKeys = envelopeDef.keys.intersect(Set(FieldModel.Glossary.kEnvelopePolesDef,
            FieldModel.Glossary.kEnvelopePolesPackedDef))
        if (extractedPoleDefKeys.size != 1)
            throw new FieldException(Cell.ErrorCodes.EnvelopeDefinitionInvalid)

        val extractedPoleVals: Vector[(Int, Int)] = extractedPoleDefKeys.head match
        {
            case FieldModel.Glossary.kEnvelopePolesDef =>
                (envelopeDef \ FieldModel.Glossary.kEnvelopePolesDef).validate[Vector[JsObject]] match
                {
                    case JsSuccess(value, _) => ExtractPolesDef(value)

                    case JsError(errors) => throw new FieldException(Cell.ErrorCodes.EnvelopeDefinitionInvalid)
                }

            case FieldModel.Glossary.kEnvelopePolesPackedDef =>
                (envelopeDef \ FieldModel.Glossary.kEnvelopePolesPackedDef).validate[String] match
                {
                    case JsSuccess(value, _) => UnpackPoles(value)

                    case JsError(errors) => throw new FieldException(Cell.ErrorCodes.EnvelopeDefinitionInvalid)
                }
        }
        if (extractedPoleVals.size > 5)
            throw new FieldException(Cell.ErrorCodes.EnvelopeDefinitionInvalid)

        // Ensure optimized pole values configuration:
        val optimumPoleVals: mutable.ArrayBuffer[(Int, Int)] = mutable.ArrayBuffer.empty[(Int, Int)]
        var lastX: Int = -1
        var lastY = lastX
        // Prepend min pole if not present:
        if (extractedPoleVals.head._1 != 0)
        {
            optimumPoleVals += ((0, 0))
            lastX = 0
        }
        // Ignore poles that are not ascending in X:
        extractedPoleVals.foreach(poleVal =>
        {
            val (x, y) = poleVal
            if (x > lastX)
            {
                optimumPoleVals += poleVal
                lastX = x
                lastY = y
            }
        })
        // Append max pole if not present:
        if (extractedPoleVals.last._1 != 15)
            optimumPoleVals += ((15, lastY))

        // Make envelope segments:
        for (index <- 0 until optimumPoleVals.size - 1)
        {
            val segmentStartX: Real = optimumPoleVals(index)._1
            val segmentStartY: Real = optimumPoleVals(index)._2
            val segmentEndX: Real = optimumPoleVals(index + 1)._1
            val segmentEndY: Real = optimumPoleVals(index + 1)._2
            val segmentSlope = (segmentEndY - segmentStartY) / (segmentEndX - segmentStartX)

            newEnvelope._segments += ((
                segmentStartX / 15,
                segmentStartY / 15,
                segmentSlope))
        }

        newEnvelope
    }

    def DecodeDiscreteEnvelopeDef (envelopeDef: JsObject): Envelope.DiscreteEnvelope =
    {
        val newEnvelope = new Envelope.DiscreteEnvelope()

        // Extract pole definitions:
        val extractedPoleDefKeys = envelopeDef.keys.intersect(Set(FieldModel.Glossary.kEnvelopePolesDef,
            FieldModel.Glossary.kEnvelopePolesPackedDef))
        if (extractedPoleDefKeys.size != 1)
            throw new FieldException(Cell.ErrorCodes.EnvelopeDefinitionInvalid)

        val extractedPoleVals: Vector[(Int, Int)] = extractedPoleDefKeys.head match
        {
            case FieldModel.Glossary.kEnvelopePolesDef =>
                (envelopeDef \ FieldModel.Glossary.kEnvelopePolesDef).validate[Vector[JsObject]] match
                {
                    case JsSuccess(value, _) => ExtractPolesDef(value)

                    case JsError(errors) => throw new FieldException(Cell.ErrorCodes.EnvelopeDefinitionInvalid)
                }

            case FieldModel.Glossary.kEnvelopePolesPackedDef =>
                (envelopeDef \ FieldModel.Glossary.kEnvelopePolesPackedDef).validate[String] match
                {
                    case JsSuccess(value, _) => UnpackPoles(value)

                    case JsError(errors) => throw new FieldException(Cell.ErrorCodes.EnvelopeDefinitionInvalid)
                }
        }
        if (extractedPoleVals.size > 5)
            throw new FieldException(Cell.ErrorCodes.EnvelopeDefinitionInvalid)

        // Ensure optimized pole values configuration:
        val optimumPoleVals: mutable.ArrayBuffer[(Int, Int)] = mutable.ArrayBuffer.empty[(Int, Int)]
        var lastX: Int = -1
        var lastY = lastX
        // Prepend min pole if not present:
        if (extractedPoleVals.head._1 != 0)
        {
            optimumPoleVals += ((0, 0))
            lastX = 0
        }
        // Ignore poles that are not ascending in X or are repeating in Y:
        extractedPoleVals.foreach(poleVal =>
        {
            val (x, y) = poleVal
            if (x > lastX && y != lastY)
            {
                optimumPoleVals += poleVal
                lastX = x
                lastY = y
            }
        })

        // Make envelope segments:
        for (index <- optimumPoleVals.indices)
        {
            val (x, y) = optimumPoleVals(index)
            newEnvelope._segments += ((x.toFloat / 15, y))
        }

        newEnvelope
    }

    def ModulateWithContinuousEnvelope (
        scalar: Continuous,
        envelope: ContinuousEnvelope): Continuous =
    {
        import scala.util.control.Breaks._

        var modulatedScalar: Continuous = 0f
        breakable
        {
            // Iterate all segments:
            envelope._segments.indices.foreach(index =>
            {
                // Determine trial X value:
                val trialX: Continuous =
                    if (index < envelope._segments.size - 1)
                        envelope._segments(index + 1)._1
                    else
                        1f
                // If scalar is less than or equal to trial X then we've found scalar in current segment:
                if (scalar <= trialX)
                {
                    val (segmentX, segmentY, segmentSlope) = envelope._segments(index)

                    // Calculate unit scalar given segment's X, Y and slope:
                    val unitScalar = (scalar - segmentX) * segmentSlope + segmentY
                    // Adjust unit scalar for ceiling and floor values:
                    val ceiling = envelope._ceiling
                    val floor = envelope._floor
                    modulatedScalar = unitScalar * (ceiling - floor) + floor
                    // Short iteration loop:
                    break()
                }
            })
        }

        modulatedScalar
    }

    def ModulateWithDiscreteEnvelope (
        scalar: Continuous,
        envelope: DiscreteEnvelope): Discrete =
    {
        import scala.util.control.Breaks._

        var modulatedScalar: Discrete = 0
        breakable
        {
            // Iterate all segments:
            envelope._segments.indices.foreach(index =>
            {
                // Determine trial X value:
                val trialX: Continuous =
                    if (index < envelope._segments.size - 1)
                        envelope._segments(index + 1)._1
                    else
                        1f
                // If scalar is less than or equal to trial X then we've found scalar in current segment:
                if (scalar <= trialX)
                {
                    val (_, segmentY) = envelope._segments(index)

                    // Modulated scalar is segment's Y value:
                    modulatedScalar = segmentY
                    // Short iteration loop:
                    break()
                }
            })
        }

        modulatedScalar
    }
}
