with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Main is

   -- Define an enum to represent the type of data stored in the union
   type DataType is (Type_Int, Type_Float, Type_Double, Type_Unknown);

   -- Define a record to hold the data and its type
   type Value_Record is record
      Type_Of_Data : DataType;
      Double_Value : Long_Float;
   end record;

   -- Function to perform arithmetic on an int
   function Process_Int (Value : Long_Float) return Long_Float is
   begin
      return Value + 10.0; -- Example: add 10 to the int
   end Process_Int;

   -- Function to perform arithmetic on a float
   function Process_Float (Value : Long_Float) return Long_Float is
   begin
      return Value * 2.0; -- Example: multiply the float by 2
   end Process_Float;

   -- Function to perform arithmetic on a double
   function Process_Double (Value : Long_Float) return Long_Float is
   begin
      return Value / 2.0; -- Example: divide the double by 2
   end Process_Double;

   -- Function to process the value based on its type
   procedure Process_Value (V : in out Value_Record) is
   begin
      case V.Type_Of_Data is
         when Type_Int =>
            V.Double_Value := Process_Int(V.Double_Value);
         when Type_Float =>
            V.Double_Value := Process_Float(V.Double_Value);
         when Type_Double =>
            V.Double_Value := Process_Double(V.Double_Value);
         when Type_Unknown =>
            Put_Line("Unsupported type");
      end case;
   end Process_Value;

   -- Function to read the data type from the config file
   function Read_Config (Filename : String) return DataType is
      File : File_Type;
      Line : Unbounded_String;
   begin
      Open (File, In_File, Filename);
      Line := To_Unbounded_String(Get_Line(File));
      Close (File);

      if Line = "int" then
         return Type_Int;
      elsif Line = "float" then
         return Type_Float;
      elsif Line = "double" then
         return Type_Double;
      else
         return Type_Unknown;
      end if;
   exception
      when others =>
         Put_Line("Failed to open or read config file");
         return Type_Unknown;
   end Read_Config;

   -- Main variables
   Config_File : constant String := "config.txt";
   Type_Of_Data : DataType;
   Value : Value_Record;
   Start_Time : Time;
   End_Time : Time;
   Elapsed_Time : Duration;
   Iterations : constant Integer := 1_000_000_000;

begin
   -- Read the data type from the config file
   Type_Of_Data := Read_Config(Config_File);

   if Type_Of_Data = Type_Unknown then
      Put_Line("Invalid or unsupported data type in config file");
      return;
   end if;

   -- Initialize the value based on the type
   Value.Type_Of_Data := Type_Of_Data;
   case Type_Of_Data is
      when Type_Int =>
         Value.Double_Value := 42.0; -- Example int value
      when Type_Float =>
         Value.Double_Value := 3.14; -- Example float value
      when Type_Double =>
         Value.Double_Value := 2.718281828459045; -- Example double value
      when Type_Unknown =>
         null;
   end case;

   -- Start the clock
   Start_Time := Clock;

   -- Process the value 1,000,000,000 times
   for I in 1 .. Iterations loop
      Process_Value(Value);
   end loop;

   -- Stop the clock
   End_Time := Clock;

   -- Calculate the elapsed time in seconds
   Elapsed_Time := To_Duration(End_Time - Start_Time);

   -- Print the result and the time taken
   case Type_Of_Data is
      when Type_Int =>
         Put_Line("Processed int: " & Long_Float'Image(Value.Double_Value));
      when Type_Float =>
         Put_Line("Processed float: " & Long_Float'Image(Value.Double_Value));
      when Type_Double =>
         Put_Line("Processed double: " & Long_Float'Image(Value.Double_Value));
      when Type_Unknown =>
         null;
   end case;
   Put_Line("Time taken for" & Integer'Image(Iterations) & " iterations: " & Duration'Image(Elapsed_Time) & " seconds");

end Main;
