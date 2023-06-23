--  Copyright (c) 2021 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Streams.Stream_IO;
with League.Strings;

with Network.Addresses;
with Network.Managers;
with Network.Connections;

package Listeners is

   type Listener is limited new Network.Managers.Connection_Listener
     and Network.Connections.Listener
   with record
      Remote  : Network.Connections.Connection;
      Output  : Ada.Streams.Stream_IO.File_Type;
      Done    : Boolean := False;
   end record;

   overriding procedure Connected
     (Self       : in out Listener;
      Connection : in out Network.Connections.Connection;
      Remote     : Network.Addresses.Address);

   overriding procedure Rejected
     (Self   : in out Listener;
      Error  : League.Strings.Universal_String;
      Remote : Network.Addresses.Address);

   overriding procedure Closed
     (Self  : in out Listener;
      Error : League.Strings.Universal_String);

   overriding procedure Can_Write (Self : in out Listener);

   overriding procedure Can_Read (Self : in out Listener);

end Listeners;
