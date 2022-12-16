module Web.View.ElephantsterGroups.New where
import Web.View.Prelude

data NewView = NewView { elephantsterGroup :: ElephantsterGroup }

instance View NewView where
    html NewView { .. } = [hsx|
        {breadcrumb}
        <h1>New ElephantsterGroup</h1>
        {renderForm elephantsterGroup}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "ElephantsterGroups" ElephantsterGroupsAction
                , breadcrumbText "New ElephantsterGroup"
                ]

renderForm :: ElephantsterGroup -> Html
renderForm elephantsterGroup = formFor elephantsterGroup [hsx|
    {(textField #creatorId)}
    {(textField #name)}
    {(textField #budget)}
    {(textField #sharedSecret)}
    {submitButton}

|]