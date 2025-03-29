with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Main is

   -- Define an enum to represent the type of data stored in the union
   type Data_Type is (Type_Int, Type_Float, Type_Double, Type_Unknown);

   -- Define a record to hold the data and its type
   type Data_Record (Type_Of_Data : Data_Type := Type_Unknown) is record
      case Type_Of_Data is
         when Type_Int =>
            Int_Value : Integer;
         when Type_Float =>
            Float_Value : Float;
         when Type_Double =>
            Double_Value : Long_Float;
         when Type_Unknown =>
            null;
      end case;
   end record;

   -- Function to perform arithmetic on an int
   function Process_Int (Value : Integer) return Integer is
   begin
      return Value + 10; -- Example: add 10 to the int
   end Process_Int;

   -- Function to perform arithmetic on a float
   function Process_Float (Value : Float) return Float is
   begin
      return Value * 2.0; -- Example: multiply the float by 2
   end Process_Float;

   -- Function to perform arithmetic on a double
   function Process_Double (Value : Long_Float) return Long_Float is
   begin
      return Value / 2.0; -- Example: divide the double by 2
   end Process_Double;

   -- Function to process the value based on its type
   procedure Process_Value (V : in out Data_Record) is
   begin
      case V.Type_Of_Data is
         when Type_Int =>
            V.Int_Value := Process_Int(V.Int_Value);
         when Type_Float =>
            V.Float_Value := Process_Float(V.Float_Value);
         when Type_Double =>
            V.Double_Value := Process_Double(V.Double_Value);
         when Type_Unknown =>
            Put_Line("Unsupported type");
      end case;
   end Process_Value;

   -- Function to read the data type from the config file
   function Read_Config (Filename : String) return Data_Type is
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
   Type_Of_Data : Data_Type;
   Data_Value : Data_Record; -- Default discriminant (Type_Unknown)
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
   case Type_Of_Data is
      when Type_Int =>
         Data_Value := (Type_Of_Data => Type_Int, Int_Value => 42); -- Example int value
      when Type_Float =>
         Data_Value := (Type_Of_Data => Type_Float, Float_Value => 3.14); -- Example float value
      when Type_Double =>
         Data_Value := (Type_Of_Data => Type_Double, Double_Value => 2.718281828459045); -- Example double value
      when Type_Unknown =>
         null;
   end case;

   -- Start the clock
   Start_Time := Clock;

   -- Process the value 1,000,000,000 times
   for I in 1 .. Iterations loop
      Process_Value(Data_Value);
   end loop;

   -- Stop the clock
   End_Time := Clock;

   -- Calculate the elapsed time in seconds
   Elapsed_Time := To_Duration(End_Time - Start_Time);

   -- Print the result and the time taken
   case Type_Of_Data is
      when Type_Int =>
         Put_Line("Processed int: " & Integer'Image(Data_Value.Int_Value));
      when Type_Float =>
         Put_Line("Processed float: " & Float'Image(Data_Value.Float_Value));
      when Type_Double =>
         Put_Line("Processed double: " & Long_Float'Image(Data_Value.Double_Value));
      when Type_Unknown =>
         null;
   end case;
   Put_Line("Time taken for" & Integer'Image(Iterations) & " iterations: " & Duration'Image(Elapsed_Time) & " seconds");

end Main;
