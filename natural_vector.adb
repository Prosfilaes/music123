package body Natural_Vector is
   function Advance (Old_Size : Natural) return Natural is
   begin
      if Old_Size < 250 then
         return 500;
      elsif Old_Size < 1_000_000 then
         return Old_Size * 2;
      else
         return Old_Size + 1_000_000;
      end if;
   end Advance;
end Natural_Vector;
