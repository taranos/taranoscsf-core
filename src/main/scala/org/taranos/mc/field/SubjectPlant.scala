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

package org.taranos.mc.field

import org.taranos.mc.Cell
import org.taranos.mc.trunk.intraprocess.TrunkModel


class SubjectPlant
    (implicit
        protected val _fieldModel: FieldModel,
        protected val _trunkModel: TrunkModel)
    extends FieldElementPlant
{
    import scala.collection.mutable

    private
    val _subjects = mutable.HashMap.empty[(Field.Key, Subject.Key), Subject]

    def CreateSubject (
        field: Field,
        constructor: Subject.Constructor): Subject =
    {
        // 1: Create element:
        val subject = new Subject(
            new Subject.Meta(
                FieldElement.MakeUniqueKey(constructor._tag),
                constructor._tag,
                constructor._badgeOpt,
                constructor._nameOpt,
                constructor._descriptionOpt),
            new Subject.Refs(field.GetKey),
            new Subject.State(
                constructor._position,
                constructor._rotation))

        // 2: Add element to store:
        _subjects += (field.GetKey, subject.GetKey) -> subject

        // 3: Bind with/create children:
        // Create default emitter:
        val emitterConstructor = new SubjectEmitter.Constructor(
            _tag = constructor._tag + FieldModel.Glossary.kTagSeparator + FieldModel.Glossary.kEDefaultEmitter,
            _patchDefOpt = constructor._patchDefOpt,
            _modulatorKeyOpt = constructor._modulatorKeyOpt)
        _fieldModel.CreateSubjectEmitters(
            field.GetKey,
            subject.GetKey,
            Vector(emitterConstructor))

        // 4: Bind with peers:
        // N/A

        // 5: Bind with parent:
        field.BindSubject(subject.GetKey)

        // 6: Activate element:
        // N/A

        // Return element:
        subject
    }

    def DestroySubject (
        field: Field,
        destructor: Subject.Destructor): Subject.Key =
    {
        _subjects.get((field.GetKey, destructor._key)) match
        {
            case Some(subject) =>
                // 1: Unbind with parent:
                field.UnbindSubject(destructor._key)

                // 2: Deactivate element:
                // N/A

                // 3: Unbind with peers:
                // N/A

                // 4: Unbind with/destroy children:
                val subjectEmitterDestructors =
                    subject.GetEmitterKeys.map(new SubjectEmitter.Destructor(_, destructor._scope))
                if (subjectEmitterDestructors.nonEmpty)
                    _fieldModel.DestroySubjectEmitters(
                        field.GetKey,
                        subjectEmitterDestructors.toVector,
                        isForcedDestroy = true)

                // 5: Remove element from store:
                _subjects -= ((field.GetKey, destructor._key))

            case None => throw new FieldException(Cell.ErrorCodes.SubjectUnknown)
        }

        // Return subject key:
        destructor._key
    }

    def DestroyAllSubjects (
        field: Field,
        scope: Symbol): Unit =
    {
        val fieldKey = field.GetKey

        _subjects.filter(_._1._1 == fieldKey).foreach(subjectPair =>
        {
            val ((_, pairSubjectKey), _) = subjectPair
            val subjectDestructor = new Subject.Destructor(pairSubjectKey, scope)
            DestroySubject(field, subjectDestructor)
        })
    }

    def GetSubjectOpt (
        field: Field,
        key: Subject.Key,
        isRequired: Boolean = true): Option[Subject] =
    {
        // Lookup subject:
        key match
        {
            case _: Subject.Key =>
                val opt = _subjects.get((field.GetKey, key))
                if (isRequired && opt.isEmpty)
                    throw new FieldException(Cell.ErrorCodes.SubjectUnknown)
                opt

            case _ => throw new FieldException(Cell.ErrorCodes.SubjectKeyInvalid)
        }
    }

    def GetSubjects (field: Field): Vector[Subject] =
    {
        val fieldKey = field.GetKey

        // Return subjects vector:
        _subjects.filter(_._1._1 == fieldKey).values.toVector
    }

    def GetElementCount (fieldKey: Field.Key): Int = _subjects.size
}
