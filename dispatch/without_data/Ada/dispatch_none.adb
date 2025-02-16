with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;

procedure Main is

   function Process_Double (Value : Long_Float) return Long_Float is
   begin
      return Value / 2.0; -- Example: divide the double by 2
   end Process_Double;

   Value : Long_Float := 2.718281828459045; -- Example double value

   Start_Time : Time;
   End_Time : Time;
   Elapsed_Time : Duration;

   Iterations : constant Integer := 1_000_000_000;
begin

   Start_Time := Clock;

   for I in 1 .. Iterations loop
      Value := Process_Double(Value);
   end loop;

   End_Time := Clock;

   Elapsed_Time := To_Duration(End_Time - Start_Time);

   Put_Line("Processed double: " & Long_Float'Image(Value));
   Put_Line("Time taken for" & Integer'Image(Iterations) & " iterations: " & Duration'Image(Elapsed_Time) & " seconds");

end Main;
