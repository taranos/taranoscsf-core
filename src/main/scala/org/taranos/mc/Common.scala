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

package org.taranos.mc

import play.api.libs.json.{JsObject, Json}


object Common
{
    type Integer = Int

    type Real = Double

    trait Propertyset
        extends Reportable

    class ReportSectionsParser (sectionsOpt: Option[String])
    {
        val kBitHasMetaPropertyset = 1 << 0
        val kBitHasAttrsPropertyset = 1 << 1
        val kBitHasRefsPropertyset = 1 << 2
        val kBitHasStatePropertyset = 1 << 3
        val kBitHasGeoPropertyset = 1 << 4
        val kBitHasChildReports = 1 << 5
        val kBitHasPeerReports = 1 << 6

        // Current policy is to always include meta section in reports:
        var flags = kBitHasMetaPropertyset

        if (sectionsOpt.isDefined)
        {
            for (sectionChar: Char <- sectionsOpt.get)
                flags |= (sectionChar match
                {
                    case 'm' => kBitHasMetaPropertyset  // meta propertyset section

                    case 'a' => kBitHasAttrsPropertyset // attrs propertyset section

                    case 'r' => kBitHasRefsPropertyset  // refs propertyset section

                    case 's' => kBitHasStatePropertyset // state propertyset section

                    case 'g' => kBitHasGeoPropertyset   // geometry property section

                    case 'c' => kBitHasChildReports     // include child reports

                    case 'p' => kBitHasPeerReports      // include peer reports
                })
        }

        def HasMetaPropertyset: Boolean = (flags & kBitHasMetaPropertyset) > 0

        def HasAttrsPropertyset: Boolean = (flags & kBitHasAttrsPropertyset) > 0

        def HasRefsPropertyset: Boolean = (flags & kBitHasRefsPropertyset) > 0

        def HasStatePropertyset: Boolean = (flags & kBitHasStatePropertyset) > 0

        def HasGeoPropertyset: Boolean = (flags & kBitHasGeoPropertyset) > 0

        def HasChildReports: Boolean = (flags & kBitHasChildReports) > 0

        def HasPeerReports: Boolean = (flags & kBitHasPeerReports) > 0
    }

    object Reportable
    {
        def ReportInteger (integer: Integer) = "%d".format(integer)

        def ReportReal1 (real: Real) = "%.1f".format(real)

        def ReportReal3 (real: Real) = "%.3f".format(real)
    }

    trait Reportable
    {
        def Report (sections: ReportSectionsParser): JsObject = Json.obj()
    }

    trait Reporter
        extends Reportable
    {
        protected[mc]
        def Report (sectionsOpt: Option[String]): JsObject
    }
}
