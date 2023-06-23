--  SPDX-FileCopyrightText: 2023 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

package body Network.Connections.Internal is

   ----------
   -- Cast --
   ----------

   function Cast
     (Self : access Network.Abstract_Connections.Abstract_Connection'Class)
      return Connection
   is
   begin
      Self.Reference;
      return (Ada.Finalization.Controlled with Object => Self);
   end Cast;

end Network.Connections.Internal;
