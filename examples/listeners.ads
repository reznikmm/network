--  Copyright (c) 2021 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Streams.Stream_IO;
with League.Strings;

with Network.Connections;
with Network.Addresses;

package Listeners is

   type Listener (Connect : not null Network.Connections.Connection_Access)
   is limited new Network.Connections.Event_Listener with record
      Output : Ada.Streams.Stream_IO.File_Type;
   end record;

   overriding procedure Connected
     (Self   : in out Listener;
      Remote : Network.Addresses.Address);

   overriding procedure Closed
     (Self  : in out Listener;
      Error : League.Strings.Universal_String);

   overriding procedure Can_Write (Self : in out Listener);

   overriding procedure Can_Read (Self : in out Listener);

end Listeners;
