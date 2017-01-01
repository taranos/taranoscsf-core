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


//
// VectorN
//

trait VectorN

//
// Vector1
//

class Vector1 (
    var _x: Double = 0f)
    extends VectorN
{
    def Set (newValue: Vector1): Unit =
    {
        _x = newValue._x
    }
}

//
// Vector2
//

class Vector2 (
    var _x: Double = 0f,
    var _y: Double = 0f)
    extends VectorN
{
    def Set (newValue: Vector2): Unit =
    {
        _x = newValue._x
        _y = newValue._y
    }
}

//
// Vector3
//

class Vector3 (
    var _x: Double = 0f,
    var _y: Double = 0f,
    var _z: Double = 0f)
    extends VectorN
{
    def Set (newValue: Vector3): Unit =
    {
        _x = newValue._x
        _y = newValue._y
        _z = newValue._z
    }
}

//
// Vector4
//

class Vector4 (
    var _w: Double = 0f,
    var _x: Double = 0f,
    var _y: Double = 0f,
    var _z: Double = 0f)
    extends VectorN
{
    def Set (newValue: Vector4): Unit =
    {
        _w = newValue._w
        _x = newValue._x
        _y = newValue._y
        _z = newValue._z
    }
}

object Common
{}
