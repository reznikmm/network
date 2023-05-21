with Ada.Streams.Stream_IO;

with League.Strings;

with Network.Addresses;
with Network.Connections;
with Network.Managers;

package Echo_Listeners is

   type Connection_Listener is limited new
     Network.Managers.Connection_Listener with null record;

   overriding procedure Connected
     (Self       : in out Connection_Listener;
      Connection : not null Network.Connections.Connection_Access;
      Remote     : Network.Addresses.Address);

   type Stream_Listener is limited new Network.Connections.Listener
   with record
      Connection : not null Network.Connections.Connection_Access;
      Output     : Ada.Streams.Stream_IO.File_Type;
   end record;

   type Stream_Listener_Access is access all Stream_Listener;

   overriding procedure Closed
     (Self  : in out Stream_Listener;
      Error : League.Strings.Universal_String);

   overriding procedure Can_Read (Self : in out Stream_Listener);

end Echo_Listeners;
