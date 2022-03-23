A quick test to repro the issue found in #587

  $ ./build.sh
  Starting type_of pass
  Adding (root Odoc_bug__).A_intf to env
  Adding (root Odoc_bug__).B to env
  Adding (root Odoc_bug__).B_intf to env
  Finished type_of pass
  Adding (root Odoc_bug__).A_intf to env
  Adding (root Odoc_bug__).B to env
  Adding (root Odoc_bug__).B_intf to env
  Starting type_of pass
  Adding (root Odoc_bug__a_intf).S.Foo to env
  Finished type_of pass
  Adding (root Odoc_bug__a_intf).S.Foo to env
  Adding (root Odoc_bug__a_intf).S.Foo to env
  Starting type_of pass
  Adding (root Odoc_bug__b_intf).Bar to env
  Adding (root Odoc_bug__b_intf).S.{Foo}1 to env
  Adding (root Odoc_bug__b_intf).S.Foo to env
  Handling include in type_of
  Removing (root Odoc_bug__b_intf).S.{Foo}1 from env
  Finished handling include in type_of
  Adding (root Odoc_bug__b_intf).B.Foo to env
  Handling include in type_of
  Removing (root Odoc_bug__b_intf).B.Foo from env
  Finished handling include in type_of
  Finished type_of pass
  Adding (root Odoc_bug__b_intf).Bar to env
  Adding (root Odoc_bug__b_intf).S.{Foo}1 to env
  Adding (root Odoc_bug__b_intf).S.Foo to env
  Handling include of : unresolvedroot(Odoc_bug__).A_intf.S
  Removing (root Odoc_bug__b_intf).S.{Foo}1 from env
  Removing (root Odoc_bug__b_intf).S.{Foo}1 from env
  Adding (root Odoc_bug__b_intf).S.{Foo}2 to env
  Adding (root Odoc_bug__b_intf).S.{Foo}1 to env
  Adding (root Odoc_bug__b_intf).S.Foo to env
  Handling include of : unresolvedroot(Odoc_bug__).A_intf.S
  Removing (root Odoc_bug__b_intf).S.{Foo}1 from env
  Removing (root Odoc_bug__b_intf).S.{Foo}1 from env
  Adding (root Odoc_bug__b_intf).S.{Foo}3 to env
  Adding (root Odoc_bug__b_intf).B.Foo to env
  Handling include of : identifier((root Odoc_bug__b_intf).B.S, false)
  Removing (root Odoc_bug__b_intf).B.Foo from env
  Removing (root Odoc_bug__b_intf).B.Foo from env
  Adding (root Odoc_bug__b_intf).B.{Foo}2 to env
  Adding (root Odoc_bug__b_intf).B.Foo to env
  Adding (root Odoc_bug__b_intf).B.Foo to env
  Handling include of : identifier((root Odoc_bug__b_intf).B.S, false)
  Removing (root Odoc_bug__b_intf).B.Foo from env
  Removing (root Odoc_bug__b_intf).B.Foo from env
  Adding (root Odoc_bug__b_intf).B.{Foo}2 to env
  Adding (root Odoc_bug__b_intf).B.Foo to env
  Starting type_of pass
  Adding (root Odoc_bug__b).Foo to env
  Handling include in type_of
  Removing (root Odoc_bug__b).Foo from env
  Finished handling include in type_of
  Finished type_of pass
  Adding (root Odoc_bug__b).Foo to env
  Handling include of : unresolvedroot(Odoc_bug__).B_intf.B
  Removing (root Odoc_bug__b).Foo from env
  Removing (root Odoc_bug__b).Foo from env
  Adding (root Odoc_bug__b).{Foo}2 to env
  Adding (root Odoc_bug__b).Foo to env
  Starting type_of pass
  Adding (root Odoc_bug__c).Foo to env
  Handling include in type_of
  Removing (root Odoc_bug__c).Foo from env
  Finished handling include in type_of
  Finished type_of pass
  Adding (root Odoc_bug__c).Foo to env
  Handling include of : unresolvedroot(Odoc_bug__).B.S
  Removing (root Odoc_bug__c).Foo from env
  Removing (root Odoc_bug__c).Foo from env
  Adding (root Odoc_bug__c).{Foo}3 to env
  Adding (root Odoc_bug__c).Foo to env
