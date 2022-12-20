module Web.Controller.ElephantsterGroups where

import Web.Controller.Prelude
import Web.View.ElephantsterGroups.Index
import Web.View.ElephantsterGroups.New
import Web.View.ElephantsterGroups.Edit
import Web.View.ElephantsterGroups.Show
import Web.JsonTypes
import qualified Data.Map.Strict as Map


instance Controller ElephantsterGroupsController where
    beforeAction = ensureIsUser

    action ElephantsterGroupsAction = do
        elephantsterGroups :: [Include "creatorId" ElephantsterGroup] <- do
            query @ElephantsterGroup
                |> innerJoin @GroupMembership (#id, #groupId)
                |> filterWhereJoinedTable @GroupMembership (#userId, currentUserId)
            |> fetch
            >>= collectionFetchRelated #creatorId

        members' :: [LabeledData (Id' "elephantster_groups") User] <- do
            query @User
                |> innerJoin @GroupMembership (#id, #userId)
                |> filterWhereInJoinedTable @GroupMembership (#groupId, (map (.id) elephantsterGroups))
                |> labelResults @GroupMembership #groupId
                |> fetch

        let membersMap :: Map (Id' "elephantster_groups") [User] =
                members'
                |> foldr (\LabeledData { labelValue, contentValue } acc -> Map.insertWith (++) labelValue [contentValue] acc) Map.empty

        let groups_ = map (\g -> groupToPrivateJSON g (Map.findWithDefault [] g.id membersMap)) elephantsterGroups
        render IndexView { groups = groups_ }

    action NewElephantsterGroupAction = do
        let elephantsterGroup = newRecord
        render NewView { .. }

    action ShowElephantsterGroupAction { elephantsterGroupId } = do
        elephantsterGroup <- fetch elephantsterGroupId
        render ShowView { .. }

    action EditElephantsterGroupAction { elephantsterGroupId } = do
        elephantsterGroup <- fetch elephantsterGroupId
        render EditView { .. }

    action UpdateElephantsterGroupAction { elephantsterGroupId } = do
        elephantsterGroup <- fetch elephantsterGroupId
        elephantsterGroup
            |> buildElephantsterGroup
            |> ifValid \case
                Left elephantsterGroup -> render EditView { .. }
                Right elephantsterGroup -> do
                    elephantsterGroup <- elephantsterGroup |> updateRecord
                    setSuccessMessage "ElephantsterGroup updated"
                    redirectTo EditElephantsterGroupAction { .. }

    action CreateElephantsterGroupAction = do
        let elephantsterGroup = newRecord @ElephantsterGroup
        elephantsterGroup
            |> buildElephantsterGroup
            |> ifValid \case
                Left elephantsterGroup -> render NewView { .. }
                Right elephantsterGroup -> do
                    elephantsterGroup <- elephantsterGroup |> createRecord
                    setSuccessMessage "ElephantsterGroup created"
                    redirectTo ElephantsterGroupsAction

    action DeleteElephantsterGroupAction { elephantsterGroupId } = do
        elephantsterGroup <- fetch elephantsterGroupId
        deleteRecord elephantsterGroup
        setSuccessMessage "ElephantsterGroup deleted"
        redirectTo ElephantsterGroupsAction

buildElephantsterGroup elephantsterGroup = elephantsterGroup
    |> fill @["creatorId","name","budget","sharedSecret"]
