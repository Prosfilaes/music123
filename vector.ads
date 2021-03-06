generic
   type Component is private;
package Vector is

   subtype Index is Natural;

   type Vector is private;

   Bad_Position : exception;

   --  If the first operation is an append, then init is unnessecary.
   function New_Vector return Vector;
   procedure Init (New_Vect : out Vector);
   procedure Reinit (Vect : in out Vector);
   procedure Append (Vect : in out Vector; Tail : Component);
   procedure Expand (Vect : in out Vector; Times : Integer := 1);
   function Get (Vect : Vector; Pos : Index) return Component;
   procedure Set (Vect : in out Vector; Pos : in Index; Item : in Component);
   procedure Remove (Vect : in out Vector; Pos : in Index);
   function Length (Vect : Vector) return Index;
   function Empty (Vect : Vector) return Boolean;
   function Advance (Old_Size : Index) return Index;

private

   pragma Inline (Append);
   pragma Inline (Expand);
   pragma Inline (Get);
   pragma Inline (Set);
   pragma Inline (Length);
   pragma Inline (Empty);

   type Real_Vector is array (Index range <>) of Component;

   type RV_Access is access Real_Vector;

   type Vector is record
      Length : Index; --  The index following the last one.
      Real_Length : Index; --  The actual length of the vector
      Vect : RV_Access;
   end record;

end Vector;
