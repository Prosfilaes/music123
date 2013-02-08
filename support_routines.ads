with Natural_Vector;
with UString_List; use UString_List;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Intl;

package Support_Routines is

   Version : constant String := Intl.Gettext ("music123 version 1 by David Starner");

   Noted_Error : exception;

   type Tool is record
      Program : Unbounded_String;
      Extension_List : Unbounded_String;
      Options : Unbounded_String;
   end record;

   package Tool_List is new Natural_Vector (Tool);
   use Tool_List;

   procedure Error (Error_String : String);
   procedure Import_Conffile (Program_List : out Tool_List.Vector);
   procedure Expand_And_Check_Filenames
     (File_List : in out UString_List.Vector;
      Option_Recurse : in Boolean;
      Extension_List : in Tool_List.Vector);
   procedure Play_Songs
     (File_List : in UString_List.Vector;
      Option_Quiet : in Boolean;
      Program_List : in Tool_List.Vector);
   procedure Randomize_Names (File_List : in out UString_List.Vector);
   procedure Read_Playlist (Full_Name : String; File_List : in out UString_List.Vector);
   function Check_Filename (Full_Name : String; Extension_List : Tool_List.Vector) return Boolean;

end Support_Routines;

