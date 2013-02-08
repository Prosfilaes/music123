with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1;
with Ada.Command_Line.Environment; use Ada.Command_Line.Environment;
with Ada.Numerics.Discrete_Random;
with Ada.Calendar;
with Ada.Strings.Fixed;

with GNAT.IO_Aux; use GNAT.IO_Aux;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

with Intl; use Intl;
with Interfaces.C;

package body Support_Routines is

   function N (Msg : String) return String renames Gettext;

   procedure Error (Error_String : String) is
   begin
      Put (Standard_Error, "music123: " & Error_String); New_Line (Standard_Error);
      Put (Standard_Error, Version); New_Line (Standard_Error);
      Put (Standard_Error, N ("usage: music123 [-hqrvz] files ..."));
      New_Line (Standard_Error);
      Put (Standard_Error, N ("-h     This help"));
      New_Line (Standard_Error);
      Put (Standard_Error, N ("-q     Run quiet"));
      New_Line (Standard_Error);
      Put (Standard_Error, N ("-r     Recurse over directories"));
      New_Line (Standard_Error);
      Put (Standard_Error, N ("-v     Print the version and exit"));
      New_Line (Standard_Error);
      Put (Standard_Error, N ("-z     Randomize the filelist"));
      New_Line (Standard_Error);
   end Error;

   No_Home_Directory : exception;

   function Home_Directory return String is
   begin
      for I in 1 .. Environment_Count loop
         declare
            E : String := Environment_Value (i);
         begin
            if E (1 .. 5) = "HOME=" then
               return (E  (6 .. E'Last));
            end if;
         end;
      end loop;
      raise No_Home_Directory;
   end Home_Directory;

   --   function Get_Line (File : File_Type) return String is
   --      Return_Value : Unbounded_String;
   --      A : String (1 .. 80);
   --      Line_Length : Integer := 80;
   --   begin
   --      while Line_Length = 80 loop
   --         Get_Line (File, A, Line_Length);
   --         Append (Return_Value, A);
   --      end loop;
   --      return To_String (Return_Value);
   --   end Get_Line;

   function Is_Whitespace (C : Character) return Boolean is
   begin
      return (C = ' ') or else (C = Ada.Characters.Latin_1.HT);
   end Is_Whitespace;

   procedure Import_Conffile (Program_List : out Tool_List.Vector)
   is
      Conf_File : File_Type;
      Pointer_Start, Pointer_End : Natural;
   begin
      Program_List := New_Vector;
      begin
         Open (Conf_File, In_File, Home_Directory & "/.music123");
         goto Conf_File_Open;
      exception
         when others =>
            null;
      end;
      begin
         Open (Conf_File, In_File, "/etc/music123rc");
      exception
         when others =>
            Error (N ("Neither /etc/music123rc or ~/.music123 found. Exiting."));
            raise Noted_Error;
      end;
      <<Conf_File_Open>> null;
      while (not End_Of_File (Conf_File)) loop
         declare
            Line : String := Get_Line (Conf_File);
            This_Tool : Tool;
         begin
            if Line'Length > 4 and then Line (Line'First .. Line'First + 3) = "tool" then
               Pointer_Start := Line'First + 4;
               while Is_Whitespace (Line (Pointer_Start)) loop
                  Pointer_Start := Pointer_Start + 1;
               end loop;
               Pointer_End := Pointer_Start;
               while not Is_Whitespace (Line (Pointer_End + 1)) loop
                  Pointer_End := Pointer_End + 1;
               end loop;
               This_Tool.Program := To_Unbounded_String (Line (Pointer_Start .. Pointer_End));
               Pointer_Start := Pointer_End + 1;
               while Is_Whitespace (Line (Pointer_Start)) loop
                  Pointer_Start := Pointer_Start + 1;
               end loop;
               Pointer_End := Pointer_Start;
               while not Is_Whitespace (Line (Pointer_End + 1)) loop
                  Pointer_End := Pointer_End + 1;
               end loop;
               This_Tool.Extension_List := To_Unbounded_String (Line (Pointer_Start .. Pointer_End));
               Pointer_Start := Pointer_End + 1;
               while (Line (Pointer_Start) /= '"') loop -- " -- This is legal, no matter what emacs says!
                  Pointer_Start := Pointer_Start + 1;
               end loop;
               Pointer_Start := Pointer_Start + 1;
               Pointer_End := Pointer_Start;
               while (Line (Pointer_End) /= '"') loop -- " -- Once again!
                  Pointer_End := Pointer_End + 1;
               end loop;
               Pointer_End := Pointer_End - 1;
               This_Tool.Options := To_Unbounded_String (Line (Pointer_Start .. Pointer_End));
               Append (Program_List, This_Tool);
            end if;
         end;
      end loop;
      Close (Conf_File);
   end Import_Conffile;

   Null_Tool : constant Tool := (To_Unbounded_String ("/dev/null"), Null_Unbounded_String, Null_Unbounded_String);

   function Matched_Extension (Extension_List : in Tool_List.Vector; Filename : String) return Tool is
      Pointer_Start, Pointer_End : Natural;
   begin
      for I in 0 .. Length (Extension_List) loop
         declare
            Ext_List : String := To_String (Get (Extension_List, I).Extension_List);
         begin
            Pointer_Start := Ext_List'First;
            Pointer_End := Ext_List'First;
            while Pointer_End < Ext_List'Last loop

               while Pointer_End /= Ext_List'Last and then Ext_List (Pointer_End + 1) /= ',' loop
                  Pointer_End := Pointer_End + 1;
               end loop;
               if Filename'Length > Pointer_End - Pointer_Start + 1 and then
                 (Filename (Filename'Last + Pointer_Start - Pointer_End - 1 .. Filename'Last) =
                  "." & Ext_List (Pointer_Start .. Pointer_End)) then
                  return Get (Extension_List, I);
               end if;
               Pointer_Start := Pointer_End + 2;
               Pointer_End := Pointer_Start;

            end loop;
         end;
      end loop;
      return Null_Tool;

   end Matched_Extension;

   function Check_Filename (Full_Name : String; Extension_List : Tool_List.Vector) return Boolean is
   begin
      return Is_Directory (Full_Name) or else
        Matched_Extension (Extension_List, Full_Name) /= Null_Tool;
   end Check_Filename;

   procedure Expand_And_Check_Filenames
     (File_List : in out UString_List.Vector;
      Option_Recurse : in Boolean;
      Extension_List : in Tool_List.Vector
     )
   is
      Current_Directory : Dir_Type;
      I : Integer;
      Directory_Entry : String (1 .. 4096);
      -- 4096 is more than FILENAME_MAX on glibc 2.1 for Linux/i386.
      -- The internal implementation of Read (Dir) only does 1024
      -- on the same platform, but this makes us safe even if they
      -- fix that.
      Filename_Length : Natural;
   begin
      if Empty (Extension_List) then
         Error (N ("The config file (~/.music123 or /etc/music123) is corrupt."));
         raise Noted_Error;
      end if;
      I := 0;
      while not Empty (File_List) and then I <= Length (File_List) loop
         --         Put (Integer'Image (Length (File_List))); New_Line;
         --         Put (Integer'Image(I)); New_Line; New_Line;
         declare
            Current_File : String := To_String (Get (File_List, I));
         begin
            if not File_Exists (Current_File) then
               Remove (File_List, I);
               I := I - 1;
            elsif Is_Directory (Current_File) then
               begin
                  Remove (File_List, I);
                  I := I - 1;
                  if Option_Recurse then
                     Open (Current_Directory, Current_File);
                     Filename_Length := 1;
                     while (Filename_Length /= 0) loop
                        Read (Current_Directory, Directory_Entry, Filename_Length);
                        if Filename_Length /= 0 and then
                          (Filename_Length /= 1 or else Directory_Entry (1) /= '.') and then
                          (Filename_Length /= 2 or else Directory_Entry (1 .. 2) /= "..") then
                           declare
                              Full_Name : String := Current_File & "/" & Directory_Entry (1 .. Filename_Length);
                           begin
                              --  Adding, then removing the file was creating a O(N^2) result. Check it first.
                              if Check_Filename (Full_Name, Extension_List) then
                                 Append (File_List, To_Unbounded_String (Full_Name));
                              end if;
                           end;
                        end if;
                     end loop;
                     Close (Current_Directory);
                  end if;
               exception
                  when Directory_Error =>
                     null;
               end;
            end if;
            I := I + 1;
         end;
      end loop;
      if Empty (File_List) then
         Error (N ("No valid filenames found."));
         raise Noted_Error;
      end if;

   end Expand_And_Check_Filenames;

   procedure Randomize_Names (File_List : in out UString_List.Vector) is
      subtype Index is Integer range 0 .. 2**16;
      package Index_Pkg is new Ada.Numerics.Discrete_Random (Index);
      use Index_Pkg;
      use Ada.Calendar;

      A, B : Unbounded_String;
      J, K : Index;
      Gen : Generator;
      Fold_Value : Integer := 2 ** 16;
      Len : Integer := Length (File_List);
   begin
      while (Index'Last / Fold_Value) < Len loop
         Fold_Value := Fold_Value / 2;
      end loop;

      Reset (Gen, Integer (Seconds (Clock) * 10.0));
      for I in 0 .. Len loop
         loop
            J := Random (Gen) / Fold_Value;
            exit when J <= Len;
         end loop;
         loop
            K := Random (Gen) / Fold_Value;
            exit when K <= Len;
         end loop;
         A := Get (File_List, J);
         B := Get (File_List, K);
         Set (File_List, J, B);
         Set (File_List, K, A);
      end loop;
   end Randomize_Names;

   function Real_Shell_Fix (File : String) return String is
   begin
--      Put (File); Put (Integer'Image (File'First)); Put (Integer'Image (File'Last)); New_Line;
      if File'Length = 1 then
         if File = "'" then
            return "'""'""'";
         else
            return File;
         end if;
      elsif File (File'First) = ''' then
         return "'""'""'" & Real_Shell_Fix (File (File'First + 1 .. File'Last));
      else
         return File (File'First) & Real_Shell_Fix (File (File'First + 1 .. File'Last));
      end if;
   end Real_Shell_Fix;

   function Shell_Fix (File : Unbounded_String) return String is
   begin
      return Real_Shell_Fix (To_String (File));
   end Shell_Fix;

   procedure Play_Songs
     (File_List : in UString_List.Vector;
      Option_Quiet : in Boolean;
      Program_List : in Tool_List.Vector) is

      This_Program : Tool;
      use Interfaces.C;
      function System (Command : Char_Array) return Integer;
      pragma Import (C, System, "system");
      System_Result : Integer;
   begin
      for I in 0 .. Length (File_List) loop
         This_Program := Matched_Extension (Program_List, To_String (Get (File_List, I)));
         if Option_Quiet then
            declare
               System_String : String := To_String (This_Program.Program & " " & This_Program.Options & " '" &
                                                    Shell_Fix (Get (File_List, I)) & "'" & ">/dev/null 2>/dev/null");
            begin
               System_Result := System (To_C (System_String));
            end;
         else
            declare
               System_String : String :=  To_String (This_Program.Program & " " & This_Program.Options & " '" &
                                                     Shell_Fix (Get (File_List, I)) & "'");
            begin
               System_Result := System (To_C (System_String));
            end;
         end if;
         delay (0.5);
      end loop;
   end Play_Songs;

   procedure Read_Playlist (Full_Name : String; File_List : in out UString_List.Vector) is
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
            Line : String := Ada.Strings.Fixed.Trim (Get_Line (Playlist), Ada.Strings.Both);
         begin
            if Line /= "" and then Line (1) /= '#' then
               Append (File_List, To_Unbounded_String (Line));
            end if;
         end;
      end loop;
      Close (Playlist);
   end Read_Playlist;

 end Support_Routines;

