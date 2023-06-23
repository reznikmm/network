with Ada.Wide_Wide_Text_IO;
with Ada.Streams.Stream_IO.C_Streams;
with Interfaces.C_Streams;

package body Echo_Listeners is

   ------------
   -- Closed --
   ------------

   overriding procedure Closed
     (Self  : in out Stream_Listener;
      Error : League.Strings.Universal_String) is
   begin
      Ada.Wide_Wide_Text_IO.Put ("Closed: ");
      Ada.Wide_Wide_Text_IO.Put_Line (Error.To_Wide_Wide_String);
      Self.Connection := Network.Connections.Null_Connection;
   end Closed;

   ---------------
   -- Connected --
   ---------------

   overriding procedure Connected
     (Self       : in out Connection_Listener;
      Connection : in out Network.Connections.Connection;
      Remote     : Network.Addresses.Address)
   is
      Listener : constant Stream_Listener_Access := new Stream_Listener'
        (Connection => Connection, Output => <>);
   begin
      Ada.Wide_Wide_Text_IO.Put ("Connected: ");
      Ada.Wide_Wide_Text_IO.Put_Line
        (Network.Addresses.To_String (Remote).To_Wide_Wide_String);
      Ada.Streams.Stream_IO.C_Streams.Open
        (Listener.Output,
         Ada.Streams.Stream_IO.Out_File,
         Interfaces.C_Streams.stdout);
      Connection.Set_Listener (Network.Connections.Listener_Access (Listener));
   end Connected;

   --------------
   -- Can_Read --
   --------------

   overriding procedure Can_Read (Self : in out Stream_Listener) is
      use type Ada.Streams.Stream_Element_Count;

      Count : Natural := 0;
      Data  : Ada.Streams.Stream_Element_Array (1 .. 512);
      Last  : Ada.Streams.Stream_Element_Count;
   begin
      Ada.Wide_Wide_Text_IO.Put_Line ("Can_Read");

      loop
         Self.Connection.Read (Data, Last);
         Ada.Streams.Stream_IO.Write (Self.Output, Data (1 .. Last));
         exit when Last < Data'First;
         Count := Count + 1;
      end loop;

      Ada.Wide_Wide_Text_IO.Put_Line
        (Count'Wide_Wide_Image & Last'Wide_Wide_Image);
   end Can_Read;

end Echo_Listeners;
