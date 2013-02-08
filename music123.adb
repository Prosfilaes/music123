-- .TH music123 1 "July 29, 2000"
--
-- .SH NAME
-- music123 \- plays various sound files (usually including MP3, OGG and Wav).
--
-- .SH SYNOPSIS
-- .B music123
-- [
-- .B -hqrvz
-- ]
-- .I file
-- .B ...
--
-- .SH DESCRIPTION
-- .B music123
-- is a shell around various command line programs to play music files.
-- It will descend directories trees with -r, and randomize file lists
-- with -z. The programs used and the options given them are listed
-- in /etc/music123rc or ~/.music123rc.
--
-- .SH OPTIONS
-- .IP -h
-- Show command help and exit;
-- .IP -q
-- Quiet mode.  No messages are displayed.
-- .IP -r
-- Recurse into directories, instead of ignoring them.
-- .IP -v
-- Display version information and exit.
-- .IP -z
-- Play files in random order.
-- .IP --
-- End option list.
--
-- .SH EXAMPLES
--
-- Play three songs:
-- .RS
-- .B music123 test1.ogg test2.mp3 test3.wav
-- .RE
-- .PP
--
-- Play a couple of directories and other songs:
-- .RS
-- .B music123 -r Rock/ test1.ogg Pop/ test4.wav
-- .RE
-- .PP
--
-- .SH FILES
--
-- .TP
-- /etc/music123rc
-- Describes which programs
-- .B music123
-- uses, which files types it supports,
-- and which options it passes those programs.
--
-- .TP
-- ~/.music123rc
-- Per-user config file to override the system wide settings.
-- .PP
--
-- .SH AUTHORS
--
-- .TP
-- Authors:
-- .br
-- David Starner <dstarner98@aasaa.ofe.org>
-- .br

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;

with Support_Routines; use Support_Routines;
use Support_Routines.Tool_List;
with Gtkada.Intl; use Gtkada.Intl;
with UString_List; use UString_List;

procedure Music123 is
   Arg_Num : Positive;
   Option_Quiet : Boolean := False;
   Option_Recurse : Boolean := False;
   Option_Random : Boolean := False;
   File_List : UString_List.Vector := New_Vector;
   Program_List : Tool_List.Vector := New_Vector;

   function N (Msg : String) return String renames Gettext;
begin
   --  Read command-line arguments
   if Argument_Count = 0 then
      Error (N ("No arguments found."));
      Set_Exit_Status (Failure);
      return;
   end if;
   Arg_Num := 1;
   while Arg_Num <= Argument_Count loop
      if Argument (Arg_Num) = "-h" then
         Error ("");
         Set_Exit_Status (Success);
         return;
      elsif Argument (Arg_Num) = "-q" then
         Option_Quiet := True;
      elsif Argument (Arg_Num) = "-z" then
         Option_Random := True;
      elsif Argument (Arg_Num) = "-r" then
         Option_Recurse := True;
      elsif Argument (Arg_Num) = "-v" then
         Ada.Text_IO.Put (Version); Ada.Text_IO.New_Line;
         Set_Exit_Status (Success);
         return;
      elsif Argument (Arg_Num) = "--" then
         for I in Arg_Num + 1 .. Argument_Count loop
            Append (File_List, To_Unbounded_String (Argument (I)));
         end loop;
         Arg_Num := Argument_Count + 1;
      elsif Argument (Arg_Num) (1) = '-' then
         Error (N ("Unknown argument found."));
         Set_Exit_Status (Failure);
         return;
      else
         Append (File_List, To_Unbounded_String (Argument (Arg_Num)));
      end if;
      Arg_Num := Arg_Num + 1;
   end loop;

   Import_Conffile (Program_List);

   Expand_And_Check_Filenames (File_List, Option_Recurse, Program_List);

   if Option_Random then
      Randomize_Names (File_List);
   end if;
   Play_Songs (File_List, Option_Quiet, Program_List);



exception
   when Noted_Error =>
      null;
end Music123;
