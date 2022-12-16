module Web.View.ElephantsterGroups.Index where
import Web.View.Prelude
import Web.JsonTypes

data IndexView = IndexView { groups :: [PrivateGroupJSON]  }

instance View IndexView where
    html IndexView { .. } = groupListWidget groups
