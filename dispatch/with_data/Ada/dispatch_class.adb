with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Real_Time;       use Ada.Real_Time;
with Ada.Exceptions;

procedure Main is

   ----------------------------------------------------------------
   -- Nested Package: Value_Pkg
   --
   -- This package defines an abstract tagged type Value_Base and its
   -- concrete extensions for integer, float, and long-float values.
   -- (We rename "Value" to "Value_Base" to avoid name clashes.)
   ----------------------------------------------------------------
   package Value_Pkg is

      -- Abstract base type with dispatching operations.
      type Value_Base is abstract tagged null record;
      procedure Process(V : in out Value_Base) is abstract;
      procedure Print  (V : Value_Base) is abstract;

      -- Concrete type for integer values.
      type Int_Value is new Value_Base with record
         Value : Integer := 42;
      end record;
      overriding procedure Process(V : in out Int_Value);
      overriding procedure Print  (V : Int_Value);

      -- Concrete type for float values.
      type Float_Value is new Value_Base with record
         Value : Float := 3.14;
      end record;
      overriding procedure Process(V : in out Float_Value);
      overriding procedure Print  (V : Float_Value);

      -- Concrete type for double values (using Ada's Long_Float).
      type Double_Value is new Value_Base with record
         Value : Long_Float := 2.718281828459045;
      end record;
      overriding procedure Process(V : in out Double_Value);
      overriding procedure Print  (V : Double_Value);

      -- Factory function: creates the appropriate value object.
      function Create_Value(Type_Str : String) return Value_Base'Class;

   end Value_Pkg;

   ----------------------------------------------------------------
   -- Nested Package Body for Value_Pkg
   ----------------------------------------------------------------
   package body Value_Pkg is

      -- Implementation for Int_Value: add 10.
      procedure Process(V : in out Int_Value) is
      begin
         V.Value := V.Value + 10;
      end Process;

      procedure Print(V : Int_Value) is
      begin
         Put_Line("Processed int: " & Integer'Image(V.Value));
      end Print;

      -- Implementation for Float_Value: multiply by 2.
      procedure Process(V : in out Float_Value) is
      begin
         V.Value := V.Value * 2.0;
      end Process;

      procedure Print(V : Float_Value) is
      begin
         Put_Line("Processed float: " & Float'Image(V.Value));
      end Print;

      -- Implementation for Double_Value: divide by 2.
      procedure Process(V : in out Double_Value) is
      begin
         V.Value := V.Value / 2.0;
      end Process;

      procedure Print(V : Double_Value) is
      begin
         Put_Line("Processed double: " & Long_Float'Image(V.Value));
      end Print;

      -- Factory function: based on the input string, create the corresponding object.
      function Create_Value(Type_Str : String) return Value_Base'Class is
      begin
         if Type_Str = "int" then
            return Int_Value'(Value => 42);
         elsif Type_Str = "float" then
            return Float_Value'(Value => 3.14);
         elsif Type_Str = "double" then
            return Double_Value'(Value => 2.718281828459045);
         else
            raise Program_Error with "Unsupported type: " & Type_Str;
         end if;
      end Create_Value;

   end Value_Pkg;

   ----------------------------------------------------------------
   -- Declarations for configuration and timing.
   ----------------------------------------------------------------
   Config_File : constant String := "config.txt";
   -- Fixed buffer for the config line (max length 100)
   Config_Line : String (1 .. 100);
   Last        : Natural;

   Iterations : constant Natural := 1_000_000_000;
   Start_Time, End_Time : Time;
   Span     : Time_Span;
   Elapsed  : Duration;

   File : File_Type;

begin
   -- Read the configuration file.
   Open(File, In_File, Config_File);
   Get_Line(File, Config_Line, Last);
   Close(File);
   -- Instead of reassigning to the fixed-length Config_Line (which would
   -- cause a length constraint error), define an unconstrained string:
   declare
      Actual_Config : constant String := Config_Line(1 .. Last);
      Value_Obj     : Value_Pkg.Value_Base'Class :=
                        Value_Pkg.Create_Value(Actual_Config);
   begin
      Start_Time := Clock;
      for I in 1 .. Iterations loop
         Value_Pkg.Process(Value_Obj);
      end loop;
      End_Time := Clock;
      Span := End_Time - Start_Time;
      Elapsed := To_Duration(Span);

      Value_Pkg.Print(Value_Obj);
      Put_Line("Time taken for " & Natural'Image(Iterations) &
               " iterations: " & Duration'Image(Elapsed) & " seconds");
   end;

exception
   when E : others =>
      Put_Line("Error: " & Ada.Exceptions.Exception_Information(E));
      raise;
end Main;
