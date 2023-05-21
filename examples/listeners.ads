--  Copyright (c) 2021 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Streams.Stream_IO;
with League.Strings;

with Network.Connection_Promises;
with Network.Connections;

package Listeners is

   type Listener is limited new Network.Connection_Promises.Listener
     and Network.Connections.Listener
   with record
      Promise : Network.Connection_Promises.Promise;
      Remote  : Network.Connections.Connection;
      Output  : Ada.Streams.Stream_IO.File_Type;
      Done    : Boolean := False;
   end record;

   overriding procedure On_Resolve
     (Self  : in out Listener;
      Value : in out Network.Connections.Connection);

   overriding procedure On_Reject
     (Self  : in out Listener;
      Value : in out League.Strings.Universal_String);

   overriding procedure Closed
     (Self  : in out Listener;
      Error : League.Strings.Universal_String);

   overriding procedure Can_Write (Self : in out Listener);

   overriding procedure Can_Read (Self : in out Listener);

end Listeners;
