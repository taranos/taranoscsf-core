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

import org.taranos.mc.Common.Reportable
import org.taranos.mc.field.FieldModel.FieldModelReporter
import org.taranos.mc.trunk.intraprocess.TrunkModel
import play.api.libs.json.{JsObject, Json}


trait FieldElementPlant
    extends FieldModelReporter
{
    protected
    val _fieldModel: FieldModel

    protected
    val _trunkModel: TrunkModel

    def GetElementCount (fieldKey: Field.Key): Int

    def Report (fieldKey: Field.Key, sectionsOpt: Option[String] = None): JsObject =
    {
        var report = Json.obj()

        report ++= Json.obj(FieldModel.Glossary.kRElementCount -> Reportable.ReportInteger(GetElementCount(fieldKey)))

        report
    }
}
