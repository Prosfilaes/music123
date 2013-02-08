-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                        Copyright (C) 2000                         --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with System;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Intl is


   -------------
   -- Gettext --
   -------------

   function Gettext (Msg : String) return String is
      function Internal (Msg : String) return chars_ptr;
      pragma Import (C, Internal, "gettext");
   begin
      return Value (Internal (Msg & ASCII.Nul));
   end Gettext;

   --------------
   -- Dgettext --
   --------------

   function Dgettext (Domain : String; Msg : String) return String is
      function Internal (Domain, Msg : String) return chars_ptr;
      pragma Import (C, Internal, "dgettext");
   begin
      return Value (Internal (Domain & ASCII.Nul, Msg & ASCII.Nul));
   end Dgettext;

   ---------------
   -- Dcgettext --
   ---------------

   function Dcgettext
     (Domain : String; Msg : String; Category : Integer) return String
   is
      function Internal
        (Domain, Msg : String; Category : Integer) return chars_ptr;
      pragma Import (C, Internal, "dcgettext");
   begin
      return Value (Internal (Domain & ASCII.Nul, Msg & ASCII.Nul, Category));
   end Dcgettext;

   -------------------------
   -- Default_Text_Domain --
   -------------------------

   function Default_Text_Domain return String is
      function Internal (Domain : System.Address) return chars_ptr;
      pragma Import (C, Internal, "textdomain");
   begin
      return Value (Internal (System.Null_Address));
   end Default_Text_Domain;

   -----------------
   -- Text_Domain --
   -----------------

   procedure Text_Domain (Domain : String := "") is
      procedure Internal (Domain : String);
      pragma Import (C, Internal, "textdomain");
   begin
      Internal (Domain & ASCII.Nul);
   end Text_Domain;

   ----------------------
   -- Bind_Text_Domain --
   ----------------------

   procedure Bind_Text_Domain (Domain : String; Dirname : String) is
      procedure Internal (Domain, Dirname : String);
      pragma Import (C, Internal, "bindtextdomain");
   begin
      Internal (Domain & ASCII.Nul, Dirname & ASCII.Nul);
   end Bind_Text_Domain;

end Intl;
