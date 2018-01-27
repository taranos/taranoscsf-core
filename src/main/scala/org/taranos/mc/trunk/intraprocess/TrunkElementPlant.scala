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

import org.taranos.mc.Common.Reportable
import org.taranos.mc.trunk.intraprocess.TrunkModel.TrunkModelReporter
import play.api.libs.json.{JsObject, Json}


trait TrunkElementPlant
    extends TrunkModelReporter
{
    protected
    val _trunkModel: TrunkModel

    def GetElementCount (trunkKey: Trunk.Key): Int

    def Report (trunkKey: Trunk.Key, sectionsOpt: Option[String] = None): JsObject =
    {
        var report = Json.obj()

        report ++= Json.obj(TrunkModel.Glossary.kRElementCount -> Reportable.ReportInteger(GetElementCount(trunkKey)))

        report
    }
}
