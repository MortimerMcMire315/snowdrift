module Handler.User.User where

import Import

import Handler.User.Utils (checkEditUser, startEmailVerification)
import Model.User

postUserByIdR :: UserId -> Handler Html
postUserByIdR user_id = do
    user <- runDB $ get404 user_id
    void $ checkEditUser user_id
    memail <- runDB $ fetchUserEmail user_id
    case memail of
        Nothing ->
            alertDanger "No email address is associated with your account."
        Just email -> startEmailVerification user_id email
    redirect $ UserR $ userNick user
