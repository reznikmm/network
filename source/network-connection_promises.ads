--  SPDX-FileCopyrightText: 2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with League.Strings;

with Network.Connections;
with Network.Generic_Promises;

package Network.Connection_Promises is new Network.Generic_Promises
  (Resolve_Element => Network.Connections.Connection_Access,
   Reject_Element  => League.Strings.Universal_String);

pragma Preelaborate (Network.Connection_Promises);
