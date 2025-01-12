with Ada.Command_Line;
with Ada.Numerics.Discrete_Random;
with Ada.Text_IO;
with Sf;
with Sf.Audio;
with Sf.Audio.Sound;
with Sf.Audio.SoundBuffer;

with ISA;
with CPU;
with Video;

procedure Ada_Chip is
   package Random_Byte is new Ada.Numerics.Discrete_Random (ISA.Byte);

   Steps_Per_Frame : constant := 8;

   State            : CPU.Instance;
   Random_Generator : Random_Byte.Generator;
   Delay_Timer      : Natural := 0;
   Sound_Timer      : Natural := 0;

   Beep_Sound_Buffer : constant Sf.Audio.sfSoundBuffer_Ptr :=
      Sf.Audio.SoundBuffer.createFromFile ("beep.ogg");
   Beep_Sound        : constant Sf.Audio.sfSound_Ptr := Sf.Audio.Sound.create;

   procedure Draw_Sprite (VX, VY : ISA.Register_Index; N : ISA.Byte) is
      use Sf;
      use ISA;

      X, Y       : sfUint32;
      Row        : aliased Byte;
      Row_Pixels : Pixel with Address => Row'Address;
      VF         : Boolean := False;
   begin
      X := sfUint32 (State.Registers (VX));
      Y := sfUint32 (State.Registers (VY));

      for I in 0 .. N - 1 loop
         Row := State.Memory (State.Address_Register + Address (I));

         for J in 0 .. 7 loop
            if Row_Pixels (7 - J) then
               if Video.Toggle_Pixel
                  ((X + sfUint32 (J)) mod Video.Width,
                     (Y + sfUint32 (I)) mod Video.Height)
               then
                  VF := True;
               end if;
            end if;
         end loop;
      end loop;

      State.Registers (15) := (if VF then 1 else 0);
   end Draw_Sprite;

   procedure Run_Flow (ins : ISA.Opcode) is
   begin
      case ins.Value is
         when ISA.Clear_Screen => Video.Clear_Screen;
         when ISA.Ret => CPU.Ret (State);
         when others => begin
            Ada.Text_IO.Put_Line ("Machine code calls are unsupported!");
            delay 1.0;
            Video.Finish;
         end;
      end case;
   end Run_Flow;

   procedure Run_Input (ins : ISA.Opcode) is
      use ISA;
      Key : constant Video.Key := Video.Key
         (State.Registers (X_Register (ins)) mod 16);
   begin
      case Input_Class'Enum_Val (ins.Value mod 256) is
         when Key_Down =>
            if Video.Key_Down (Key) then
               CPU.Skip (State);
            end if;
         when Key_Up =>
            if Video.Key_Up (Key) then
               CPU.Skip (State);
            end if;
      end case;
   end Run_Input;

   procedure Run_Misc (ins : ISA.Opcode) is
      use ISA;
      X : constant Register_Index := X_Register (ins);
   begin
      case Misc_Class'Enum_Val (ins.Value mod 256) is
         when Get_Delay => State.Registers (X) := Byte (Delay_Timer);
         when Get_Key   => State.Registers (X) := Byte (Video.Next_Key);
         when Set_Delay => Delay_Timer := Natural (State.Registers (X));
         when Set_Sound => Sound_Timer := Natural (State.Registers (X));
         when Reg_Store => CPU.Reg_Store (State, X);
         when Reg_Load  => CPU.Reg_Load (State, X);
         when Add_Address => State.Address_Register :=
            State.Address_Register + Address (State.Registers (X));
         when Get_Font =>
            State.Address_Register := Address (State.Registers (X) mod 16) * 5;
         when Get_BCD => begin
            State.Memory (State.Address_Register) :=
               State.Registers (X) / 100;
            State.Memory (State.Address_Register + 1) :=
               State.Registers (X) / 10 mod 10;
            State.Memory (State.Address_Register + 2) :=
               State.Registers (X) mod 10;
         end;
      end case;
   end Run_Misc;

   procedure Run_Step is
      use ISA;
      ins : Opcode;
   begin
      ins := CPU.Get_Opcode (State);
      case ins.Class is
         when Flow => Run_Flow (ins);
         when Jump => CPU.Jump (State, Address (ins.Value));
         when Call => CPU.Call (State, Address (ins.Value));
         when Equal =>
            if State.Registers (X_Register (ins)) = To_Byte (ins) then
               CPU.Skip (State);
            end if;
         when Not_Equal =>
            if State.Registers (X_Register (ins)) /= To_Byte (ins) then
               CPU.Skip (State);
            end if;
         when Compare =>
            if State.Registers (X_Register (ins)) =
               State.Registers (Y_Register (ins))
            then
               CPU.Skip (State);
            end if;
         when Set_Register =>
            State.Registers (X_Register (ins)) := To_Byte (ins);
         when Add =>
            State.Registers (X_Register (ins)) :=
               State.Registers (X_Register (ins)) + To_Byte (ins);
         when Math =>
            CPU.Math (State, X_Register (ins), Y_Register (ins),
               To_Byte (ins) mod 16);
         when Not_Compare =>
            if State.Registers (X_Register (ins)) /=
               State.Registers (Y_Register (ins))
            then
               CPU.Skip (State);
            end if;
         when Set_Address =>
            State.Address_Register := Address (ins.Value);
         when Random =>
            State.Registers (X_Register (ins)) :=
               Random_Byte.Random (Random_Generator) mod To_Byte (ins);
         when Draw_Sprite =>
            Draw_Sprite (X_Register (ins), Y_Register (ins),
               To_Byte (ins) mod 16);
         when Input => Run_Input (ins);
         when Misc => Run_Misc (ins);
         when others => begin
            Ada.Text_IO.Put_Line ("Unknown instruction class!");
            Ada.Text_IO.Put_Line (Opcode_Class'Image (ins.Class));
            delay 1.0;
            Video.Finish;
         end;
      end case;
   end Run_Step;
begin
   if Ada.Command_Line.Argument_Count /= 1 then
      Ada.Text_IO.Put_Line ("usage: adachip <.c8 file>");
   else
      Video.Initialize;
      Random_Byte.Reset (Random_Generator);
      CPU.Load_File (State, Ada.Command_Line.Argument (1));
      Sf.Audio.Sound.setBuffer (Beep_Sound, Beep_Sound_Buffer);

      while Video.Is_Running loop
         Video.Display;
         Video.Poll_Events;

         if Delay_Timer > 0 then
            Delay_Timer := Delay_Timer - 1;
         end if;

         if Sound_Timer > 0 then
            Sf.Audio.Sound.play (Beep_Sound);
            Sound_Timer := Sound_Timer - 1;
         end if;

         for I in 0 .. Steps_Per_Frame loop
            Run_Step;
         end loop;
      end loop;

      Sf.Audio.Sound.destroy (Beep_Sound);
   end if;
end Ada_Chip;
