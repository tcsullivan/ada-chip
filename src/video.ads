with Sf;
with Sf.Graphics;
with Sf.Graphics.Sprite;

package Video is
   use Sf;
   use Sf.Graphics;

   type Model is (Chip_8, Super_Chip_10);
   type Key is range 0 .. 15;
   type Key_Map is array (Key'Range) of Boolean;

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

   function Key_Down (K : Key) return Boolean;
   function Key_Up (K : Key) return Boolean;
   function Is_Running return Boolean;
   function Toggle_Pixel (X, Y : sfUint32) return Boolean;
   function Next_Key return Key;

private
   Keys           : Key_Map;
   app            : sfRenderWindow_Ptr;
   Pixels         : sfImage_Ptr;
   Pixels_Texture : sfTexture_Ptr;
   Pixels_Sprite  : constant sfSprite_Ptr := Sprite.create;
end Video;
