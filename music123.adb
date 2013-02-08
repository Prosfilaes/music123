--  music123 by David Starner <dvdeug@debian.org>
--  See debian/copyright

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;

with Support_Routines; use Support_Routines;
with Intl; use Intl;

procedure Music123 is
   Arg_Num : Positive;
   Option_Quiet : Boolean := False;
   Option_Recurse : Boolean := False;
   Option_Random : Boolean := False;
   Option_Loop : Boolean := False;
   Option_Eternal_Random : Boolean := False;
   Option_Ignore_Extension_Case : Boolean := False;
   Delay_Length : Duration := 0.5;
   File_List : UString_List.Vector;
   Program_List : Tool_List.Vector;

   function N (Msg : String) return String renames Gettext;
begin
   Set_Locale;
   Text_Domain ("music123");
   Bind_Text_Domain ("music123", "/usr/share/locale");
   Version := To_Unbounded_String
     (Format_String (N ("music123 version %d by David Starner"), "16"));

   --  Import conffile first
   Import_Conffile (Program_List);

   --  Read command-line arguments
   if Argument_Count = 0 then
      Error (N ("No arguments found."));
      raise Noted_Error;
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
      elsif Argument (Arg_Num) = "-Z" then
         Option_Eternal_Random := True;
      elsif Argument (Arg_Num) = "-l" then
         Option_Loop := True;
      elsif Argument (Arg_Num) = "-r" then
         Option_Recurse := True;
      elsif Argument (Arg_Num) = "-i" then
         Option_Ignore_Extension_Case := True;
      elsif Argument (Arg_Num) = "-v" then
         Ada.Text_IO.Put (To_String (Version)); Ada.Text_IO.New_Line;
         Set_Exit_Status (Success);
         return;
      elsif Argument (Arg_Num) = "-D" then
         Delay_Length := 0.0;
      elsif Argument (Arg_Num) = "-d" then
         if Arg_Num < Argument_Count then
            begin
               Delay_Length := Duration'Value (Argument (Arg_Num + 1));
               Arg_Num := Arg_Num + 1;
            exception
               when others =>
                  Error (N ("Bad argument for -d."));
                  raise Noted_Error;
            end;
         else
            Error (N ("Missing argument for -d."));
            raise Noted_Error;
         end if;
      elsif Argument (Arg_Num) = "-@" then
         if Arg_Num < Argument_Count then
            Read_Playlist (Argument (Arg_Num + 1), File_List);
            Arg_Num := Arg_Num + 1;
         else
            Error (N ("Missing argument for -@."));
            raise Noted_Error;
         end if;
      elsif Argument (Arg_Num) = "--" then
         for I in Arg_Num + 1 .. Argument_Count loop
            if Check_Filename (Argument (I),
                               Program_List,
                               Option_Ignore_Extension_Case) then
               File_List.Append (Argument (I));
            end if;
         end loop;
         Arg_Num := Argument_Count + 1;
      elsif Argument (Arg_Num) (1) = '-' then
         Error (N ("Unknown argument found."));
         raise Noted_Error;
      else
         if Check_Filename (Argument (Arg_Num),
                            Program_List,
                            Option_Ignore_Extension_Case) then
            File_List.Append (Argument (Arg_Num));
         end if;
      end if;
      Arg_Num := Arg_Num + 1;
   end loop;

   Expand_And_Check_Filenames (File_List,
                               Option_Recurse,
                               Program_List,
                              Option_Ignore_Extension_Case);

   Play_Songs
     (File_List,
      Program_List,
      Delay_Length => Delay_Length,
      Option_Quiet => Option_Quiet,
      Option_Loop => Option_Loop,
      Option_Random => Option_Random,
      Option_Eternal_Random => Option_Eternal_Random,
      Option_Ignore_Extension_Case => Option_Ignore_Extension_Case);

exception
   when Noted_Error =>
      Set_Exit_Status (Failure);
end Music123;
