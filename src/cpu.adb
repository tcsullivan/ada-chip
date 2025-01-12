with Ada.Sequential_IO;
with Bit_Ops;

package body CPU is
   function Get_Opcode (Inst : in out Instance) return Opcode is
      Op : aliased Opcode_Raw;
      Op_Record : Opcode with Address => Op'Address;
   begin
      Op := Opcode_Raw (Inst.Memory (Inst.Program_Counter)) * 2 ** 8;
      Op := Op + Opcode_Raw (Inst.Memory (Inst.Program_Counter + 1));
      Inst.Program_Counter := Inst.Program_Counter + 2;
      return Op_Record;
   end Get_Opcode;

   procedure Reg_Store (Inst : in out Instance; VX : Register_Index) is
   begin
      for I in 0 .. VX loop
         Inst.Memory (Inst.Address_Register + Address (I)) :=
            Inst.Registers (I);
      end loop;
   end Reg_Store;

   procedure Reg_Load (Inst : in out Instance; VX : Register_Index) is
   begin
      for I in 0 .. VX loop
         Inst.Registers (I) := Inst.Memory
            (Inst.Address_Register + Address (I));
      end loop;
   end Reg_Load;

   procedure Ret (Inst : in out Instance) is
   begin
      Jump (Inst, Inst.Stack.Last_Element);
      Inst.Stack.Delete_Last;
   end Ret;

   procedure Call (Inst : in out Instance; A : Address) is
   begin
      Inst.Stack.Append (Inst.Program_Counter);
      Jump (Inst, A);
   end Call;

   procedure Jump (Inst : in out Instance; A : Address) is
   begin
      Inst.Program_Counter := A;
   end Jump;

   procedure Skip (Inst : in out Instance) is
   begin
      Inst.Program_Counter := Inst.Program_Counter + 2;
   end Skip;

   procedure Math (Inst : in out Instance; VX, VY : Register_Index; N : Byte)
   is
      X : constant Byte := Inst.Registers (VX);
      Y : constant Byte := Inst.Registers (VY);
   begin
      case Math_Class'Enum_Val (N) is
         when Assign => Inst.Registers (VX) := Y;
         when Bit_Or => Inst.Registers (VX) := Bit_Ops.Bitwise_Or (X, Y);
         when Bit_And => Inst.Registers (VX) := Bit_Ops.Bitwise_And (X, Y);
         when Bit_Xor => Inst.Registers (VX) := Bit_Ops.Bitwise_Xor (X, Y);
         when Add =>
            Inst.Registers (VX) := X + Y;
            Inst.Registers (15) :=
               (if Integer (X) + Integer (Y) > Integer (X + Y)
                  then 1 else 0);
         when Sub_Y =>
            Inst.Registers (VX) := X - Y;
            Inst.Registers (15) := (if X >= Y then 1 else 0);
         when Shift_Right =>
            Inst.Registers (15) := X mod 2;
            Inst.Registers (VX) := X / 2;
         when Sub_X =>
            Inst.Registers (VX) := Y - X;
            Inst.Registers (15) := (if Y >= X then 1 else 0);
         when Shift_Left =>
            Inst.Registers (15) := X / (2 ** 7);
            Inst.Registers (VX) := X * 2;
      end case;
   end Math;

   procedure Load_File (Inst : in out Instance; File_Name : String) is
      package Byte_IO is new Ada.Sequential_IO (Byte);

      I : Address := Start_Address;
      File_Handle : Byte_IO.File_Type;
   begin
      Byte_IO.Open (File_Handle, Byte_IO.In_File, File_Name);

      while not Byte_IO.End_Of_File (File_Handle) loop
         Byte_IO.Read (File_Handle, Inst.Memory (I));
         I := I + 1;
      end loop;

      Byte_IO.Close (File_Handle);
   end Load_File;
end CPU;
