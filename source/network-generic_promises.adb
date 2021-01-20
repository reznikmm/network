with Ada.Unchecked_Deallocation;

package body Network.Generic_Promises is

   procedure Free is new Ada.Unchecked_Deallocation
     (List_Node, List_Node_Access);

   function Has_Listener
     (Self  : Promise'Class;
      Value : not null Listener_Access) return Boolean;

   ------------------
   -- Add_Listener --
   ------------------

   not overriding procedure Add_Listener
     (Self  : in out Promise'Class;
      Value : not null Listener_Access)
   is
      Parent : constant Controller_Access := Self.Parent;
   begin
      case Parent.Data.State is
         when Pending =>
            if not Self.Has_Listener (Value) then
               Parent.Data.Listeners := new List_Node'
                 (Item => Value,
                  Next => Parent.Data.Listeners);
            end if;
         when Resolved =>
            Value.On_Resolve (Parent.Data.Resolve_Value);
         when Rejected =>
            Value.On_Reject (Parent.Data.Reject_Value);
      end case;
   end Add_Listener;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Controller) is
   begin
      if Self.Data.State = Pending then
         declare
            Next : List_Node_Access := Self.Data.Listeners;
         begin
            while Next /= null loop
               declare
                  Item : List_Node_Access := Next;
               begin
                  Next := Item.Next;
                  Free (Item);
               end;
            end loop;
         end;
      end if;
   end Finalize;

   ------------------
   -- Has_Listener --
   ------------------

   function Has_Listener
     (Self  : Promise'Class;
      Value : not null Listener_Access) return Boolean
   is
      Next : List_Node_Access := Self.Parent.Data.Listeners;
   begin
      while Next /= null loop
         if Next.Item = Value then
            return True;
         end if;

         Next := Next.Next;
      end loop;

      return False;
   end Has_Listener;

   -----------------
   -- Is_Attached --
   -----------------

   function Is_Attached (Self : Promise'Class) return Boolean is
   begin
      return Self.Parent /= null;
   end Is_Attached;

   ----------------
   -- Is_Pending --
   ----------------

   function Is_Pending (Self : Promise'Class) return Boolean is
   begin
      return Self.Parent.Is_Pending;
   end Is_Pending;

   function Is_Pending (Self : Controller'Class) return Boolean is
   begin
      return Self.Data.State = Pending;
   end Is_Pending;

   -----------------
   -- Is_Rejected --
   -----------------

   function Is_Rejected (Self : Promise'Class) return Boolean is
   begin
      return Self.Parent.Is_Rejected;
   end Is_Rejected;

   function Is_Rejected (Self : Controller'Class) return Boolean is
   begin
      return Self.Data.State = Rejected;
   end Is_Rejected;

   -----------------
   -- Is_Resolved --
   -----------------

   function Is_Resolved (Self : Promise'Class) return Boolean is
   begin
      return Self.Parent.Is_Resolved;
   end Is_Resolved;

   function Is_Resolved (Self : Controller'Class) return Boolean is
   begin
      return Self.Data.State = Resolved;
   end Is_Resolved;

   ------------
   -- Reject --
   ------------

   procedure Reject (Self  : in out Controller'Class;
                     Value : Reject_Element) is
      Next : List_Node_Access := Self.Data.Listeners;
   begin
      Self.Data := (Rejected, Value);

      while Next /= null loop
         declare
            Item : List_Node_Access := Next;
         begin
            Item.Item.On_Reject (Self.Data.Reject_Value);
            Next := Item.Next;
            Free (Item);
         end;
      end loop;
   end Reject;

   ---------------------
   -- Remove_Listener --
   ---------------------

   procedure Remove_Listener
     (Self  : in out Promise'Class;
      Value : not null Listener_Access)
   is
      Next : List_Node_Access := Self.Parent.Data.Listeners;
   begin
      if Next.Item = Value then
         Self.Parent.Data.Listeners := Next.Next;
         Free (Next);
      end if;

      while Next /= null loop
         if Next.Next /= null
           and then Next.Next.Item = Value
         then
            Next.Next := Next.Next.Next;
            Free (Next);

            exit;
         end if;
      end loop;
   end Remove_Listener;

   -------------
   -- Resolve --
   -------------

   procedure Resolve (Self  : in out Controller'Class;
                      Value : Resolve_Element) is
      Next : List_Node_Access := Self.Data.Listeners;
   begin
      Self.Data := (Resolved, Value);

      while Next /= null loop
         declare
            Item : List_Node_Access := Next;
         begin
            Item.Item.On_Resolve (Self.Data.Resolve_Value);
            Next := Item.Next;
            Free (Item);
         end;
      end loop;
   end Resolve;

   ----------------
   -- To_Promise --
   ----------------

   function To_Promise (Self : access Controller'Class) return Promise is
   begin
      return (Parent => Self.all'Unchecked_Access);
   end To_Promise;
end Network.Generic_Promises;
