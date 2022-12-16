module Web.View.ElephantsterGroups.Show where
import Web.View.Prelude

data ShowView = ShowView { elephantsterGroup :: ElephantsterGroup }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <h1>Show ElephantsterGroup</h1>
        <p>{elephantsterGroup}</p>

    |]
        where
            breadcrumb = renderBreadcrumb
                            [ breadcrumbLink "ElephantsterGroups" ElephantsterGroupsAction
                            , breadcrumbText "Show ElephantsterGroup"
                            ]