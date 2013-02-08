with Vector;

generic
   type Component is private;
package Natural_Vector is
   function Advance (Old_Size : Natural) return Natural;
   package NVector is new Vector (Natural, Component, Advance);

   subtype Vector is NVector.Vector;

   Bad_Position : exception renames NVector.Bad_Position;
   function New_Vector return Vector renames NVector.New_Vector;
   procedure Init (New_Vect : out Vector) renames NVector.Init;
   procedure Reinit (Vect : in out Vector) renames NVector.Reinit;
   procedure Append (Vect : in out Vector; Tail : Component)
     renames NVector.Append;
   procedure Expand (Vect : in out Vector; Times : Integer := 1)
     renames NVector.Expand;
   function Get (Vect : Vector; Pos : Natural) return Component
     renames NVector.Get;
   procedure Set (Vect : in out Vector; Pos : in Natural; Item : in Component)
     renames NVector.Set;
   procedure Remove (Vect : in out Vector; Pos : in Natural)
     renames NVector.Remove;
   function Length (Vect : Vector) return Natural renames NVector.Length;
   function Empty (Vect : Vector) return Boolean renames NVector.Empty;

   pragma Inline (Append);
   pragma Inline (Expand);
   pragma Inline (Get);
   pragma Inline (Set);
   pragma Inline (Length);
   pragma Inline (Empty);

end Natural_Vector;
