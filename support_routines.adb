with Ada.Text_IO;
with Ada.Characters.Latin_1;
with Ada.Environment_Variables;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Ada.Calendar; use Ada.Calendar;
with Ada.Strings.Fixed;
with Ada.Characters.Handling;
with Ada.Directories;

with Intl; use Intl;
with Interfaces.C;

package body Support_Routines is

   --  local function and procedures declarations
   function Matched_Extension (Extension_List : in Tool_List.Vector;
                               Filename : String;
                               Option_Ignore_Extension_Case : Boolean)
                              return Tool;
   function Is_Whitespace (C : Character) return Boolean;
   function Home_Directory return String;
   function Shell_Fix (File : String) return String;
   --  end of declarations

   function Format_String (Format : String; Insert : String) return String is
   begin
      for I in Format'First .. Format'Last - 1 loop
         if Format (I) = '%' and then Format (I + 1) = 'd' then
            return Format (Format'First .. I - 1) &
              Insert & Format (I + 2 .. Format'Last);
         end if;
      end loop;
      return Format; --  XXX Raise exception instead???
   end Format_String;

   procedure Error (Error_String : String) is
      use Ada.Text_IO;
   begin
      Put (Standard_Error, N ("music123: ") & Error_String);
      New_Line (Standard_Error);
      Put (Standard_Error, To_String (Version));
      New_Line (Standard_Error);
      Put (Standard_Error, N ("usage: music123 [-hqrvz] files ..."));
      New_Line (Standard_Error);
      Put (Standard_Error, N ("-h     This help"));
      New_Line (Standard_Error);
      Put (Standard_Error, N ("-q     Run quiet"));
      New_Line (Standard_Error);
      Put (Standard_Error, N ("-i     Ignore extension case"));
      New_Line (Standard_Error);
      Put (Standard_Error, N ("-r     Recurse over directories"));
      New_Line (Standard_Error);
      Put (Standard_Error, N ("-v     Print the version and exit"));
      New_Line (Standard_Error);
      Put (Standard_Error, N ("-z     Randomize the filelist"));
      New_Line (Standard_Error);
      New_Line (Standard_Error);
      Put (Standard_Error, N ("See the manpage for more options."));
      New_Line (Standard_Error);
   end Error;

   No_Home_Directory : exception;

   function Home_Directory return String is
      use Ada.Environment_Variables;
   begin
      if Exists ("HOME") then
         return Value ("HOME");
      else
         raise No_Home_Directory;
      end if;
   end Home_Directory;

   function Is_Whitespace (C : Character) return Boolean is
   begin
      return (C = ' ') or else (C = Ada.Characters.Latin_1.HT);
   end Is_Whitespace;

   procedure Import_Conffile (Program_List : in out Tool_List.Vector)
   is
      use Ada.Text_IO;
      Error_String : constant String
        := N ("Neither /etc/music123rc or ~/.music123rc found. Exiting.");
      Conf_File : File_Type;
      Pointer_Start, Pointer_End : Natural;
   begin
      Program_List.Clear;
      begin
         Open (Conf_File, In_File, Home_Directory & "/.music123rc");
      exception
         when others =>
            begin
               Open (Conf_File, In_File, "/etc/music123rc");
            exception
               when others =>
                  Error (Error_String);
                  raise Noted_Error;
            end;
      end;
      while (not End_Of_File (Conf_File)) loop
         declare
            Line : constant String := Get_Line (Conf_File);
            This_Tool : Tool;
         begin
            if Line'Length > 4 and
              then Line (Line'First .. Line'First + 3) = "tool" then
               Pointer_Start := Line'First + 4;
               while Is_Whitespace (Line (Pointer_Start)) loop
                  Pointer_Start := Pointer_Start + 1;
               end loop;
               Pointer_End := Pointer_Start;
               while not Is_Whitespace (Line (Pointer_End + 1)) loop
                  Pointer_End := Pointer_End + 1;
               end loop;
               This_Tool.Program :=
                 To_Unbounded_String (Line (Pointer_Start .. Pointer_End));
               Pointer_Start := Pointer_End + 1;
               while Is_Whitespace (Line (Pointer_Start)) loop
                  Pointer_Start := Pointer_Start + 1;
               end loop;
               Pointer_End := Pointer_Start;
               while not Is_Whitespace (Line (Pointer_End + 1)) loop
                  Pointer_End := Pointer_End + 1;
               end loop;
               This_Tool.Extension_List :=
                 To_Unbounded_String (Line (Pointer_Start .. Pointer_End));
               Pointer_Start := Pointer_End + 1;
               while Line (Pointer_Start) /= '"' loop
                  Pointer_Start := Pointer_Start + 1;
               end loop;
               Pointer_Start := Pointer_Start + 1;
               Pointer_End := Pointer_Start;
               while (Line (Pointer_End) /= '"') loop -- " -- Once again!
                  Pointer_End := Pointer_End + 1;
               end loop;
               Pointer_End := Pointer_End - 1;
               This_Tool.Options :=
                 To_Unbounded_String (Line (Pointer_Start .. Pointer_End));
               Program_List.Append (This_Tool);
            end if;
         end;
      end loop;
      Close (Conf_File);
   end Import_Conffile;

   Null_Tool : constant Tool := (To_Unbounded_String ("/dev/null"),
                                 Null_Unbounded_String,
                                 Null_Unbounded_String);

   function Matched_Extension (Extension_List : in Tool_List.Vector;
                               Filename : String;
                               Option_Ignore_Extension_Case : Boolean)
                              return Tool is
      Pointer_Start, Pointer_End : Natural;
   begin
      for I in Extension_List.First_Index .. Extension_List.Last_Index loop
         declare
            Ext_List : constant String :=
              To_String (Extension_List.Element (I).Extension_List);
         begin
            Pointer_Start := Ext_List'First;
            Pointer_End := Ext_List'First;
            while Pointer_End < Ext_List'Last loop

               while Pointer_End /= Ext_List'Last and then
                 Ext_List (Pointer_End + 1) /= ',' loop
                  Pointer_End := Pointer_End + 1;
               end loop;
               declare
                  Extension_String : constant String :=
                    "." & Ext_List (Pointer_Start .. Pointer_End);
               begin
                  if Filename'Length > Pointer_End - Pointer_Start + 1 then
                     declare
                        End_File_Name : constant String :=
                          Filename (Filename'Last + Pointer_Start -
                                      Pointer_End - 1 .. Filename'Last);
                        use Ada.Characters.Handling;
                     begin
                        if End_File_Name = Extension_String then
                           return Extension_List.Element (I);
                        end if;
                        if Option_Ignore_Extension_Case then
                           if To_Lower (End_File_Name) =
                             To_Lower (Extension_String) then
                              return Extension_List.Element (I);
                           end if;
                        end if;
                     end;
                  end if;
               end;
               Pointer_Start := Pointer_End + 2;
               Pointer_End := Pointer_Start;

            end loop;
         end;
      end loop;
      return Null_Tool;

   end Matched_Extension;

   function Check_Filename (Full_Name : String;
                            Extension_List : Tool_List.Vector;
                            Option_Ignore_Extension_Case : Boolean)
                           return Boolean is
      use Ada.Directories;
   begin
      return Exists (Full_Name) and then
        (Kind (Full_Name) = Directory or else
           Matched_Extension (Extension_List,
                              Full_Name,
                              Option_Ignore_Extension_Case) /= Null_Tool);
   end Check_Filename;

   procedure Expand_And_Check_Filenames
     (File_List : in out UString_List.Vector;
      Option_Recurse : in Boolean;
      Extension_List : in Tool_List.Vector;
      Option_Ignore_Extension_Case : in Boolean
     )
   is
      use Ada.Directories;
      Temporary_File_List : UString_List.Vector;
      procedure Check_And_Append (Directory_Entry : in Directory_Entry_Type);
      procedure Check_And_Append (Directory_Entry : in Directory_Entry_Type)
      is
         --  Adding, then removing the file was
         --  creating a O(N^2) result. Check it first.
         Simple : constant String := Simple_Name (Directory_Entry);
         Full_Path : constant String := Full_Name (Directory_Entry);
      begin
         if Simple /= "."
           and then Simple /= ".."
           and then Check_Filename (Full_Path,
                                    Extension_List,
                                    Option_Ignore_Extension_Case)
         then
            Temporary_File_List.Append (Full_Path);
         end if;
      end Check_And_Append;
      I : Integer;
      Error_String : constant String :=
        N ("The config file (~/.music123rc or /etc/music123rc) is corrupt.");
   begin
      if Extension_List.Is_Empty then
         Error (Error_String);
         raise Noted_Error;
      end if;
      I := File_List.First_Index;
      while I <= File_List.Last_Index loop
         declare
            Current_File : constant String := File_List.Element (I);
         begin
            if not Exists (Current_File) then
               File_List.Delete (I);
            elsif Kind (Current_File) = Directory then
               File_List.Delete (I);
               if Option_Recurse then
                  begin
                     Temporary_File_List.Clear;
                     Search (Directory => Current_File,
                             Pattern => "",
                             Process => Check_And_Append'Access);
                     File_Sorting.Sort (Temporary_File_List);
                     File_List.Append (Temporary_File_List);
                     Temporary_File_List.Clear;
                  exception
                     --  Name_Error should not happen here
                     when Use_Error => null;   --  no recurse after all
                  end;
               end if;
            else
               I := I + 1;
            end if;
         end;
      end loop;
      if File_List.Is_Empty then
         Error (N ("No valid filenames found."));
         raise Noted_Error;
      end if;

   end Expand_And_Check_Filenames;

   procedure Randomize_Names (File_List : in out UString_List.Vector) is

      J : Positive;
      Gen : Generator;
   begin
      Reset (Gen, Integer (Seconds (Clock) * 10.0));
      --  From Knuth, TAOCP vol. 2, edition 3, page 146, 3.4.2, algorithm P
      for I in reverse File_List.First_Index + 1 .. File_List.Last_Index loop
         J := Integer
           (Float'Floor (Random (Gen)
                           * Float (I + File_List.First_Index - 1)))
           + File_List.First_Index;
         File_List.Swap (I, J);
      end loop;
   end Randomize_Names;

   function Shell_Fix (File : String) return String is
   begin
      if File'Length = 1 then
         if File = "'" then
            return "'""'""'";
         else
            return File;
         end if;
      elsif File (File'First) = ''' then
         return "'""'""'" & Shell_Fix (File (File'First + 1 .. File'Last));
      else
         return File (File'First) &
           Shell_Fix (File (File'First + 1 .. File'Last));
      end if;
   end Shell_Fix;

   procedure Play_Songs
     (File_List : in out UString_List.Vector;
      Program_List : in Tool_List.Vector;
      Delay_Length : in Duration;
      Option_Quiet : in Boolean;
      Option_Loop : in Boolean;
      Option_Random : in Boolean;
      Option_Eternal_Random : in Boolean;
      Option_Ignore_Extension_Case : in Boolean
     ) is

      use Interfaces.C;
      function System (Command : char_array) return Integer;
      pragma Import (C, System, "system");
      procedure Play_A_Song (File_Name : in String;
                             Option_Quiet : in Boolean);
      procedure Play_A_Song (File_Name : in String;
                             Option_Quiet : in Boolean) is
         pragma Warnings (Off);
         System_Result : Integer;
         pragma Warnings (On);
         This_Program : Tool;
         Name_Start : Integer;
         Name_End : Integer;
         Name_End_Set : Boolean := False;
         Printable_Name : String := File_Name;
      begin
         This_Program := Matched_Extension (Program_List,
                                            File_Name,
                                            Option_Ignore_Extension_Case);
         --  XXX:Surely there's string function to do this?
         Name_Start := Printable_Name'Last - 1;
         while (Name_Start >= Printable_Name'First and then
                Printable_Name (Name_Start) /= '/')
         loop
            if not Name_End_Set and Printable_Name (Name_Start) = '.' then
               Name_End := Name_Start - 1;
               Name_End_Set := True;
            elsif Printable_Name (Name_Start) = '_' then
               Printable_Name (Name_Start) := ' ';
            end if;
            Name_Start := Name_Start - 1;
         end loop;
         Name_Start := Name_Start + 1;
         --  It should be impossible to get here, as all filenames are going
         --  to have an extension. If it's out of debugging, this should be
         --  deleted; better to not run xtermset than crash.
         if not Name_End_Set then
            raise Program_Error;
         end if;
         System_Result := System (To_C ("xtermset -T '" &
                          Shell_Fix (Printable_Name (Name_Start .. Name_End))
                          & "'"));
         if Option_Quiet then
            declare
               System_String : constant String :=
                 To_String (This_Program.Program & " " &
                              This_Program.Options & " '" &
                              Shell_Fix (File_Name) & "'" &
                              ">/dev/null 2>/dev/null");
            begin
               System_Result := System (To_C (System_String));
            end;
         else
            declare
               System_String : constant String :=
                 To_String (This_Program.Program & " " & This_Program.Options &
                              " '" & Shell_Fix (File_Name) & "'");
            begin
               System_Result := System (To_C (System_String));
            end;
         end if;
      end Play_A_Song;

      Gen : Generator;
   begin
      if Option_Eternal_Random then
         Reset (Gen, Integer (Seconds (Clock) * 10.0));
         declare
            Len : constant Natural
              := File_List.Last_Index - File_List.First_Index + 1;
            Song_Number : Positive;
         begin
            loop
               Song_Number :=
                 Integer (Float'Floor (Random (Gen) * Float (Len))) + 1;
               Play_A_Song (File_List.Element (Song_Number),
                            Option_Quiet);
               delay (Delay_Length);
            end loop;
         end;
      end if;

      if Option_Random then
         Randomize_Names (File_List);
      end if;

      loop
         for I in File_List.First_Index .. File_List.Last_Index loop
            Play_A_Song (File_List.Element (I), Option_Quiet);
            delay (Delay_Length);
         end loop;
         exit when not Option_Loop;
      end loop;
   end Play_Songs;

   procedure Read_Playlist (Full_Name : String;
                            File_List : in out UString_List.Vector) is
      use Ada.Text_IO;
      Playlist : File_Type;
   begin
      begin
         Open (Playlist, In_File, Full_Name);
      exception
         when others =>
            Error (N ("Playlist file not found."));
            raise Noted_Error;
      end;
      while (not End_Of_File (Playlist)) loop
         declare
            Line : constant String :=
              Ada.Strings.Fixed.Trim (Get_Line (Playlist),
                                      Ada.Strings.Both);
         begin
            if Line /= "" and then Line (1) /= '#' then
               File_List.Append (Line);
            end if;
         end;
      end loop;
      Close (Playlist);
   end Read_Playlist;

end Support_Routines;

