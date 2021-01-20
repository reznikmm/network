--  SPDX-FileCopyrightText: 2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Network.Addresses;
with Network.Streams;

package Network.Connections is
   pragma Preelaborate;

   type Connection is limited interface and Network.Streams.Stream;
   --  A network connection.

   type Connection_Access is access all Connection'Class
     with Storage_Size => 0;

   not overriding function Remote (Self : Connection)
     return Network.Addresses.Address is abstract;
   --  An address of the connection remote part

end Network.Connections;
