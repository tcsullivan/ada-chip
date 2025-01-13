package ISA is
   type Byte           is mod 2 ** 8;
   type Address        is mod 2 ** 12;
   type Register_Index is mod 2 ** 4;
   type Bank           is array (Address) of Byte;
   type Register_Bank  is array (Register_Index) of Byte;
   type Pixel          is array (0 .. Byte'Size - 1) of Boolean;
   pragma Pack (Pixel);

   type Opcode_Value is mod 2 ** 12;
   type Opcode_Raw   is mod 2 ** 16;
   type Opcode_Class is (
      Flow, Jump, Call, Equal, Not_Equal, Compare, Set_Register, Add, Math,
      Not_Compare, Set_Address, Jump_Relative, Random, Draw_Sprite, Input, Misc
   ) with Size => 4;
   type Opcode is record
      Value : Opcode_Value;
      Class : Opcode_Class;
   end record with
      Size => Opcode_Raw'Size;

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
      Misc          => 15
   );

   type Flow_Class is (
      Scroll_Down_0, Scroll_Down_1, Scroll_Down_2, Scroll_Down_3,
      Scroll_Down_4, Scroll_Down_5, Scroll_Down_6, Scroll_Down_7,
      Scroll_Down_8, Scroll_Down_9, Scroll_Down_10, Scroll_Down_11,
      Scroll_Down_12, Scroll_Down_13, Scroll_Down_14, Scroll_Down_15,
      Clear_Screen, Ret, Scroll_Right, Scroll_Left, Exit_Interpreter, Low_Res,
      High_Res
   );

   for Flow_Class use (
      Scroll_Down_0  => 16#C0#,
      Scroll_Down_1  => 16#C1#,
      Scroll_Down_2  => 16#C2#,
      Scroll_Down_3  => 16#C3#,
      Scroll_Down_4  => 16#C4#,
      Scroll_Down_5  => 16#C5#,
      Scroll_Down_6  => 16#C6#,
      Scroll_Down_7  => 16#C7#,
      Scroll_Down_8  => 16#C8#,
      Scroll_Down_9  => 16#C9#,
      Scroll_Down_10 => 16#CA#,
      Scroll_Down_11 => 16#CB#,
      Scroll_Down_12 => 16#CC#,
      Scroll_Down_13 => 16#CD#,
      Scroll_Down_14 => 16#CE#,
      Scroll_Down_15 => 16#CF#,
      Clear_Screen   => 16#E0#,
      Ret            => 16#EE#,
      Scroll_Right   => 16#FB#,
      Scroll_Left    => 16#FC#,
      Exit_Interpreter => 16#FD#,
      Low_Res        => 16#FE#,
      High_Res       => 16#FF#
   );

   type Math_Class is (
      Assign, Bit_Or, Bit_And, Bit_Xor, Add, Sub_Y, Shift_Right, Sub_X,
      Shift_Left
   );

   for Math_Class use (
      Assign      => 0,
      Bit_Or      => 1,
      Bit_And     => 2,
      Bit_Xor     => 3,
      Add         => 4,
      Sub_Y       => 5,
      Shift_Right => 6,
      Sub_X       => 7,
      Shift_Left  => 14
   );

   type Input_Class is (Key_Down, Key_Up);

   for Input_Class use (
      Key_Down => 16#9E#,
      Key_Up   => 16#A1#
   );

   type Misc_Class is (
      Get_Delay, Get_Key, Set_Delay, Set_Sound, Add_Address, Get_Font,
      Get_Font_10, Get_BCD, Reg_Store, Reg_Load, Reg_Store_X, Reg_Load_X
   );

   for Misc_Class use (
      Get_Delay   => Opcode_Value (16#07#),
      Get_Key     => Opcode_Value (16#0A#),
      Set_Delay   => Opcode_Value (16#15#),
      Set_Sound   => Opcode_Value (16#18#),
      Add_Address => Opcode_Value (16#1E#),
      Get_Font    => Opcode_Value (16#29#),
      Get_Font_10 => Opcode_Value (16#30#),
      Get_BCD     => Opcode_Value (16#33#),
      Reg_Store   => Opcode_Value (16#55#),
      Reg_Load    => Opcode_Value (16#65#),
      Reg_Store_X => Opcode_Value (16#75#),
      Reg_Load_X  => Opcode_Value (16#85#)
   );

   Start_Address : constant Address := 16#200#;

   function To_Byte (O : Opcode) return Byte;

   function X_Register (O : Opcode) return Register_Index;
   function Y_Register (O : Opcode) return Register_Index;
end ISA;
