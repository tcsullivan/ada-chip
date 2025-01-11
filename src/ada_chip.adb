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
   use Sf.Audio;

   package Random_Byte is new Ada.Numerics.Discrete_Random (ISA.Byte);

   Steps_Per_Frame : constant := 12;

   State            : CPU.Instance;
   Random_Generator : Random_Byte.Generator;
   Delay_Timer      : ISA.Byte := 0;
   Sound_Timer      : ISA.Byte := 0;

   Beep_Sound_Buffer : constant sfSoundBuffer_Ptr :=
      SoundBuffer.createFromFile ("beep.ogg");
   Beep_Sound        : constant sfSound_Ptr := Sound.create;

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

   procedure Run_Step is
      use ISA;

      ins : Opcode;
   begin
      ins := CPU.Get_Opcode (State);
      case ins.Class is
         when Flow => case Byte (ins.Value) is
            when ISA.Clear_Screen => Video.Clear_Screen;
            when ISA.Ret => CPU.Ret (State);
            when others => begin
               Ada.Text_IO.Put_Line ("Unknown flow instruction!");
               Ada.Text_IO.Put_Line (Opcode_Value'Image (ins.Value));
               delay 1.0;
               Video.Finish;
            end;
         end case;
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
         when Input => case To_Byte (ins) is
            when ISA.Key_Down =>
               if Video.Key_Down
                  (Video.Key (State.Registers (X_Register (ins)) mod 16))
               then
                  CPU.Skip (State);
               end if;
            when ISA.Key_Up =>
               if Video.Key_Up
                  (Video.Key (State.Registers (X_Register (ins)) mod 16))
               then
                  CPU.Skip (State);
               end if;
            when others => null;
         end case;
         when Misc => case To_Byte (ins) is
            when ISA.Get_Delay =>
               State.Registers (X_Register (ins)) := Delay_Timer;
            when ISA.Get_Key =>
               State.Registers (X_Register (ins)) := Byte (Video.Next_Key);
            when ISA.Set_Delay =>
               Delay_Timer := State.Registers (X_Register (ins));
            when ISA.Set_Sound =>
               Sound_Timer := State.Registers (X_Register (ins));
            when ISA.Add_Address =>
               State.Address_Register := State.Address_Register +
                  Address (State.Registers (X_Register (ins)));
            when ISA.Get_Font =>
               State.Address_Register :=
                  Address (State.Registers (X_Register (ins)) mod 16) * 5;
            when ISA.Get_BCD => begin
               State.Memory (State.Address_Register) :=
                  State.Registers (X_Register (ins)) / 100;
               State.Memory (State.Address_Register + 1) :=
                  State.Registers (X_Register (ins)) / 10 mod 10;
               State.Memory (State.Address_Register + 2) :=
                  State.Registers (X_Register (ins)) mod 10;
            end;
            when ISA.Reg_Store =>
               CPU.Reg_Store (State, X_Register (ins));
            when ISA.Reg_Load =>
               CPU.Reg_Load (State, X_Register (ins));
            when others => begin
               Ada.Text_IO.Put_Line ("Unknown misc instruction!");
               Ada.Text_IO.Put_Line (Opcode_Value'Image (ins.Value));
               delay 1.0;
               Video.Finish;
            end;
         end case;
         when others => begin
            Ada.Text_IO.Put_Line ("Unknown instruction class!");
            Ada.Text_IO.Put_Line (Opcode_Class'Image (ins.Class));
            delay 1.0;
            Video.Finish;
         end;
      end case;
   end Run_Step;

   use ISA;
begin
   if Ada.Command_Line.Argument_Count /= 1 then
      Ada.Text_IO.Put_Line ("usage: adachip <.c8 file>");
   else
      Video.Initialize;
      Random_Byte.Reset (Random_Generator);
      CPU.Load_File (State, Ada.Command_Line.Argument (1));
      Sound.setBuffer (Beep_Sound, Beep_Sound_Buffer);

      while Video.Is_Running loop
         Video.Display;
         Video.Poll_Events;

         if Delay_Timer > 0 then
            Delay_Timer := Delay_Timer - 1;
         end if;

         if Sound_Timer > 0 then
            Sound.play (Beep_Sound);
            Sound_Timer := Sound_Timer - 1;
         end if;

         for I in 0 .. Steps_Per_Frame loop
            Run_Step;
         end loop;
      end loop;

      Sound.destroy (Beep_Sound);
   end if;
end Ada_Chip;
