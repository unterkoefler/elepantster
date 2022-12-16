module Web.View.ElephantsterGroups.Edit where
import Web.View.Prelude

data EditView = EditView { elephantsterGroup :: ElephantsterGroup }

instance View EditView where
    html EditView { .. } = [hsx|
        {breadcrumb}
        <h1>Edit ElephantsterGroup</h1>
        {renderForm elephantsterGroup}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "ElephantsterGroups" ElephantsterGroupsAction
                , breadcrumbText "Edit ElephantsterGroup"
                ]

renderForm :: ElephantsterGroup -> Html
renderForm elephantsterGroup = formFor elephantsterGroup [hsx|
    {(textField #creatorId)}
    {(textField #name)}
    {(textField #budget)}
    {(textField #sharedSecret)}
    {submitButton}

|]