--  SPDX-FileCopyrightText: 2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Ada.Finalization;

generic
   type Resolve_Element is private;
   --  Type for reolving a promise

   type Reject_Element is private;
   --  Type for rejecting a promise

package Network.Generic_Promises is
   pragma Preelaborate;

   type Listener is limited interface;
   --  A listener on promise events
   type Listener_Access is access all Listener'Class with Storage_Size => 0;

   not overriding procedure On_Resolve
     (Self  : in out Listener;
      Value : in out Resolve_Element) is abstract;
   --  The promise is resolved with a Value

   not overriding procedure On_Reject
     (Self  : in out Listener;
      Value : in out Reject_Element) is abstract;
   --  The promise is rejected with a value

   type Promise is tagged private;
   --  Promise is an object to be resolved or rejected with some value

   function Is_Attached (Self : Promise'Class) return Boolean;
   --  The promise has a corresponding controlling part

   function Is_Pending (Self : Promise'Class) return Boolean
     with Pre => Self.Is_Attached;
   --  The promise has not resolved nor rejected yet

   function Is_Resolved (Self : Promise'Class) return Boolean
     with Pre => Self.Is_Attached;
   --  The promise has been resolved

   function Is_Rejected (Self : Promise'Class) return Boolean
     with Pre => Self.Is_Attached;
   --  The promise has been rejected

   procedure Add_Listener
     (Self  : in out Promise'Class;
      Value : not null Listener_Access);
   --  Let a listener to be notified when the promise is settled

   procedure Remove_Listener
     (Self  : in out Promise'Class;
      Value : not null Listener_Access);
   --  Let a listener to be notified when the promise is settled

   type Controller is tagged limited private;
   --  Controlling part of the promise

   function Is_Pending (Self : Controller'Class) return Boolean;
   --  The promise has not resolved nor rejected yet

   function Is_Resolved (Self : Controller'Class) return Boolean;
   --  The promise has been resolved

   function Is_Rejected (Self : Controller'Class) return Boolean;
   --  The promise has been rejected

   function To_Promise (Self : access Controller'Class) return Promise
     with Post => To_Promise'Result.Is_Attached;

   procedure Resolve
     (Self  : in out Controller'Class;
      Value : Resolve_Element)
     with Pre => Self.To_Promise.Is_Pending,
          Post => Self.To_Promise.Is_Resolved;
   --  Resolve the promise and notify listeners

   procedure Reject
     (Self  : in out Controller'Class;
      Value : Reject_Element)
     with Pre => Self.To_Promise.Is_Pending,
          Post => Self.To_Promise.Is_Rejected;
   --  Reject the promise. This will notify the listener

private
   type List_Node;
   type List_Node_Access is access List_Node;

   type List_Node is record
      Item : not null Listener_Access;
      Next : List_Node_Access;
   end record;

   type Promise_State is (Pending, Resolved, Rejected);

   type Promise_Data (State : Promise_State := Pending) is record
      case State is
         when Pending =>
            Listeners : List_Node_Access;
            Callback  : Listener_Access;
         when Resolved =>
            Resolve_Value : Resolve_Element;
         when Rejected =>
            Reject_Value  : Reject_Element;
      end case;
   end record;

   type Controller is new Ada.Finalization.Limited_Controlled with record
      Data : Promise_Data;
   end record;

   overriding procedure Finalize (Self : in out Controller);

   type Controller_Access is access all Controller'Class
     with Storage_Size => 0;

   type Promise is tagged record
      Parent : Controller_Access;
   end record;

end Network.Generic_Promises;
