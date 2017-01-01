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

package org.taranos.mc

import org.taranos.common.ServiceResult
import org.taranos.mc.field._
import play.api.libs.json.Json


object ManagementServices
{
    def DestroyCell (serviceArgs: Vector[String])
        (implicit _cell: Cell): ServiceResult =
    {
        // Reset cell:
        _cell.Reset()

        // Reply nothing:
        ServiceResult(0, Vector.empty[String])
    }

    def ReportCell (serviceArgs: Vector[String])
        (implicit _cell: Cell): ServiceResult =
    {
        // Decode query:
        val queryEncoded = serviceArgs.head
        val query = FieldModel.DecodeQuery(queryEncoded)

        // Report current cell:
        val cellReport = _cell.Report(query.sectionsOpt)

        // Wrap report:
        val wrapper = Json.obj("rc" -> cellReport)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with report:
        ServiceResult(0, results)
    }
}