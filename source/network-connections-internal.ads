--  SPDX-FileCopyrightText: 2023 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Network.Abstract_Connections;

package Network.Connections.Internal is
   pragma Preelaborate;

   function Cast
     (Self : access Network.Abstract_Connections.Abstract_Connection'Class)
     return Connection;

end Network.Connections.Internal;
