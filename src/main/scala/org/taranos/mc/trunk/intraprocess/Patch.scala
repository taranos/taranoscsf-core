//
// Taranos Cloud Sonification Framework: Service Core
// Copyright (C) 2018 David Hinson, Netrogen Blue LLC (dhinson@netrogenblue.com)
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

package org.taranos.mc.trunk.intraprocess

import org.taranos.mc.field.{Envelope, FieldModel}
import play.api.libs.json._


/*
Notes:

// Schema:
{
    // emitter patch definition:
    dpe:
    {
        dc: {}      // channel definitions (REQUIRED; at least 1 element; indexed by channel tag)
    }

    // channel definition:
    dc:
    {
        de: {},     // channel envelope (continuous; default = unity gain)
        dpo: {}     // oscillator patch definition (REQUIRED)
    }

    // oscillator patch definition:
    dpo:
    {
        w: ""       // waveset identifier (REQUIRED)
        de: {
            l: {},  // loudness envelope definition (continuous; default = max gain)
            p: {},  // pitch envelope definition (continuous; default = unity gain)
            r: {},  // period envelope definition (discrete; default = zero) ("trigger at least every 2^N periods")
            s: {},  // shape envelope definition (discrete; default = zero)
            t: {}   // tone envelope definition (discrete; default = zero)
        }
    }


// "Default" emitter patch:
{
    dc: {0: {dpo: {w: "default.tws"}}}
}


*/


object Patch
{
    val kDefaultPatchEnvelopeDef = Envelope.kUnityGainEnvelopeDef

    val kDefaultOscillatorPatchDef = Json.obj(
        FieldModel.Glossary.kQWavesetId -> FieldModel.Glossary.kDefaultWavesetId,
        FieldModel.Glossary.kEnvelopeDef -> Json.obj(
            FieldModel.Glossary.kQLoudness -> Envelope.kTwoThirdsGainWithDeadZoneEnvelopeDef,
            FieldModel.Glossary.kQPitch -> Envelope.kUnityGainEnvelopeDef,
            FieldModel.Glossary.kQPeriod -> Envelope.kZeroEnvelopeDef,
            FieldModel.Glossary.kQShape -> Envelope.kZeroEnvelopeDef,
            FieldModel.Glossary.kQTone -> Envelope.kZeroEnvelopeDef))

    val kDefaultEmitterPatchDef = Json.obj(
        FieldModel.Glossary.kChannelDef -> Json.obj(
            FieldModel.Glossary.kDefaultChannelTag -> Json.obj(
                FieldModel.Glossary.kEnvelopeDef -> kDefaultPatchEnvelopeDef,
                FieldModel.Glossary.kOscillatorPatchDef -> kDefaultOscillatorPatchDef)))

    class Key (uniqueKey: TrunkElement.UniqueKey, symbol: Symbol = 'Patch)
        extends SignalModulator.Key(uniqueKey, symbol)

    abstract
    class Meta[KeyType <: Key] (
        key: KeyType,
        tag: String,
        badgeOpt: Option[String],
        nameOpt: Option[String],
        descriptionOpt: Option[String])
        extends SignalModulator.Meta[KeyType](
            key,
            tag,
            badgeOpt,
            nameOpt,
            descriptionOpt,
            Signal.ModeEnum.Continuous)

    abstract
    class Attrs
        extends SignalModulator.Attrs

    abstract
    class Refs (
        trunkKey: Trunk.Key,
        tapKey: SignalTap.Key,
        isTapParent: Boolean)
        extends SignalModulator.Refs(
            trunkKey,
            tapKey,
            isTapParent)

    abstract
    class State
        extends SignalModulator.State
}

trait Patch[KeyType <: Patch.Key]
    extends SignalModulator[KeyType]
