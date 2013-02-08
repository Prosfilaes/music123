with Ada.Unchecked_Deallocation;

package body Vector is
   --  if Debug then checks end if; must be constant to be optimized properly
   --  which is the only point of Debug := False.
   Debug : constant Boolean := True;
   function Advance_N (Start : Index; Times : Integer) return Index;

   procedure Delete is new Ada.Unchecked_Deallocation (Real_Vector, RV_Access);

   function New_Vector return Vector is
      New_Real_Length : Index := Advance (Index'First);
      New_Vect : Vector;
   begin
      New_Vect.Vect := new Real_Vector (Index'First .. New_Real_Length);
      New_Vect.Length := Index'First;
      New_Vect.Real_Length := New_Real_Length;
      return New_Vect;
   end New_Vector;

   procedure Init (New_Vect : out Vector) is
      New_Real_Length : Index := Advance (Index'First);
   begin
      New_Vect.Vect := new Real_Vector (Index'First .. New_Real_Length);
      New_Vect.Length := Index'First;
      New_Vect.Real_Length := New_Real_Length;
   end Init;

   procedure Reinit (Vect : in out Vector) is
      New_Real_Length : Index := Advance (Index'First);
   begin
      Delete (Vect.Vect);
      Vect.Vect := new Real_Vector (Index'First .. New_Real_Length);
      Vect.Length := Index'First;
      Vect.Real_Length := New_Real_Length;
   end Reinit;

   function Advance_N (Start : Index; Times : Integer) return Index is
      Final_Size : Index := Start;
   begin
      for I in 1 .. Times loop
         Final_Size := Advance (Final_Size);
      end loop;
      return Final_Size;
   end Advance_N;
   pragma Inline (Advance_N);

   procedure Expand (Vect : in out Vector; Times : Integer := 1) is
      New_Real_Length : Index := Advance_N (Vect.Real_Length, Times);
      NRV_Access : RV_Access;
--      New_Real_Vect : Real_Vector
--        (Index'First .. New_Real_Length);
   begin
      NRV_Access := new Real_Vector (Index'First .. New_Real_Length);
--      NRV_Access.all := New_Real_Vect;
      if Vect.Length /= Index'First then
         for I in Index'First .. Index'Pred (Vect.Length) loop
            NRV_Access.all (I) := Vect.Vect.all (I);
         end loop;
      end if;
      Delete (Vect.Vect);
      Vect.Vect := NRV_Access;
      Vect.Real_Length := New_Real_Length;
   end Expand;

   procedure Append (Vect : in out Vector; Tail : Component) is
   begin
      if Vect.Vect = null then
         Init (Vect);
      end if;
      if Vect.Length = Vect.Real_Length then
         Expand (Vect);
      end if;
      Vect.Vect.all (Vect.Length) := Tail;
      Vect.Length := Index'Succ (Vect.Length);
   end Append;

   function Get (Vect : Vector; Pos : Index) return Component is
   begin
      if Debug and then Pos >= Vect.Length then
         raise Bad_Position;
      end if;
      return Vect.Vect.all (Pos);
   end Get;

   procedure Set (Vect : in out Vector; Pos : in Index; Item : in Component) is
   begin
      if Debug and then Pos >= Vect.Length then
         raise Bad_Position;
      end if;
      Vect.Vect.all (Pos) := Item;
   end Set;

   procedure Remove (Vect : in out Vector; Pos : in Index) is begin
      for I in Pos .. Index'Pred (Index'Pred (Vect.Length)) loop
         Set (Vect, I, Get (Vect, Index'Succ (I)));
      end loop;
      Vect.Length := Index'Pred (Vect.Length);
   end Remove;

   function Empty (Vect : Vector) return Boolean is begin
      return Vect.Length = Index'First;
   end Empty;

   function Length (Vect : Vector) return Index is begin
      return Index'Pred (Vect.Length);
   end Length;

end Vector;
