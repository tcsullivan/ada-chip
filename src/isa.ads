package ISA is
   type Byte           is mod 2 ** 8;
   type Address        is mod 2 ** 12;
   type Register_Index is mod 2 ** 4;
   type Bank           is array (Address) of Byte;
   type Register_Bank  is array (Register_Index) of Byte;
   type Pixel          is array (0 .. 7) of Boolean;
   pragma Pack (Pixel);

   type Opcode_Value is mod 2 ** 12;
   type Opcode_Class is (
      Flow, Jump, Call, Equal, Not_Equal, Compare, Set_Register, Add, Math,
      Not_Compare, Set_Address, Jump_Relative, Random, Draw_Sprite, Input, Misc
   ) with Size => 4;
   type Opcode_Raw   is mod 2 ** 16;
   type Opcode is record
      Value : Opcode_Value;
      Class : Opcode_Class;
   end record with
      Size => 16;

   for Opcode use record
      Value at 0 range 0 .. 11;
      Class at 0 range 12 .. 15;
   end record;

   for Opcode_Class use (
      Flow          => 0,
      Jump          => 1,
      Call          => 2,
      Equal         => 3,
      Not_Equal     => 4,
      Compare       => 5,
      Set_Register  => 6,
      Add           => 7,
      Math          => 8,
      Not_Compare   => 9,
      Set_Address   => 10,
      Jump_Relative => 11,
      Random        => 12,
      Draw_Sprite   => 13,
      Input         => 14,
      Misc          => 15);

   Start_Address : constant Address := 16#200#;

   function To_Byte (O : Opcode) return Byte;

   function X_Register (O : Opcode) return Register_Index;
   function Y_Register (O : Opcode) return Register_Index;
end ISA;
