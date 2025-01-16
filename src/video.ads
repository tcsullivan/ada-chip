with Sf;
with Sf.Graphics;
with Sf.Graphics.Sprite;
with Sf.Window.Keyboard;

package Video is
   use Sf;
   use Sf.Graphics;
   use Sf.Window.Keyboard;

   type Model is (Chip_8, Super_Chip_10);

   Title  : constant String := "Ada-Chip";

   Width  : sfUint32;
   Height : sfUint32;
   Scale  : sfUint32;

   procedure Clear_Screen;
   procedure Low_Res;
   procedure High_Res;
   procedure Initialize (M : Model);
   procedure Display;
   procedure Finish;
   procedure Poll_Events;

   procedure Scroll_Right;
   procedure Scroll_Left;

   function Is_Running return Boolean;
   function Toggle_Pixel (X, Y : sfUint32) return Boolean;

   type Key is (
      Unknown,
      Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine,
      A, B, C, D, E, F
   );

   for Key use (
      Unknown => -1,
      Zero  => 0,
      One   => 1,
      Two   => 2,
      Three => 3,
      Four  => 4,
      Five  => 5,
      Six   => 6,
      Seven => 7,
      Eight => 8,
      Nine  => 9,
      A     => 10,
      B     => 11,
      C     => 12,
      D     => 13,
      E     => 14,
      F     => 15
   );

   type Key_Map is array (Key'First .. Key'Last) of sfScancode;

   function Key_Down (K : Key) return Boolean;
   function Key_Up (K : Key) return Boolean;
   function Next_Key return Key;

   function Translate_Scancode (S : sfScancode) return Key;
   function Translate_Key (K : Key) return sfScancode;

private
   Last_Key       : Key;
   app            : sfRenderWindow_Ptr;
   Pixels         : sfImage_Ptr;
   Pixels_Texture : sfTexture_Ptr;
   Pixels_Sprite  : constant sfSprite_Ptr := Sprite.create;

   Key_Conv : Key_Map := (
      sfScanUnknown,
      sfScanNum0, sfScanNum1, sfScanNum2, sfScanNum3, sfScanNum4, sfScanNum5,
      sfScanNum6, sfScanNum7, sfScanNum8, sfScanNum9,
      sfScanA, sfScanB, sfScanC, sfScanD, sfScanE, sfScanF
   );
end Video;
