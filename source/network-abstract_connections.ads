--  SPDX-FileCopyrightText: 2023 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Network.Addresses;
with Network.Streams;

--  private
package Network.Abstract_Connections is
   pragma Preelaborate;

   type Abstract_Connection is limited interface
     and Network.Streams.Input_Stream
     and Network.Streams.Output_Stream;

   not overriding function Remote (Self : Abstract_Connection)
     return Network.Addresses.Address is abstract;

   not overriding procedure Reference
     (Self : in out Abstract_Connection) is abstract;

   not overriding function Dereference (Self : in out Abstract_Connection)
     return Boolean is abstract;

end Network.Abstract_Connections;
