with Ada.Containers.Vectors;
with ISA;

package CPU is
   use ISA;

   package Address_Stack is new Ada.Containers.Vectors
      (Index_Type => Natural, Element_Type => Address);

   type Instance is record
      Memory           : Bank;
      Registers        : Register_Bank;
      Program_Counter  : Address := Start_Address;
      Address_Register : Address := 0;
      Stack            : Address_Stack.Vector;
      Delay_Timer      : Byte := 0;
   end record;

   procedure Load_File (Inst : in out Instance; File_Name : String);

   function Get_Opcode (Inst : in out Instance) return Opcode;

   procedure Reg_Store (Inst : in out Instance; VX : Register_Index);
   procedure Reg_Load (Inst : in out Instance; VX : Register_Index);
   procedure Ret (Inst : in out Instance);
   procedure Call (Inst : in out Instance; A : Address);
   procedure Jump (Inst : in out Instance; A : Address);
   procedure Skip (Inst : in out Instance);
   procedure Math (Inst : in out Instance; VX, VY : Register_Index; N : Byte);
end CPU;
