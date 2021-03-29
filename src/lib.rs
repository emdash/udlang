// udlang: stream processing language.
//
// Copyright (C) 2021  Brandon Lewis
//
// This program is free software: you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public License
// as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program.  If not, see
// <https://www.gnu.org/licenses/>.
#[macro_use]
extern crate lalrpop_util;
extern crate ordered_float;

#[macro_use]
pub mod ast;
pub mod env;
lalrpop_mod!(pub grammar);
pub mod parser;
pub mod typechecker;
pub mod ir;
pub mod vm;
