module Handler.User.ProjectNotifications where

import Import

import Handler.Utils
import Handler.User.Utils (checkEditUser)
import Model.User
import View.User

-- /#UserId/project-notifications

getProjectNotificationsR :: UserId -> ProjectId -> Handler Html
getProjectNotificationsR user_id project_id = do
    void $ checkEditUser user_id
    user    <- runYDB $ get404 user_id
    project <- runYDB $ get404 project_id
    let fetchNotifPref =
            runYDB . fetchProjectNotificationPrefDB user_id project_id
    mwiki_page        <- fetchNotifPref NotifWikiPage
    mwiki_edit        <- fetchNotifPref NotifWikiEdit
    mblog_post        <- fetchNotifPref NotifBlogPost
    mnew_pledge       <- fetchNotifPref NotifNewPledge
    mupdated_pledge   <- fetchNotifPref NotifUpdatedPledge
    mdeleted_pledge   <- fetchNotifPref NotifDeletedPledge
    mvolunteer_app    <- fetchNotifPref NotifVolunteerApp
    is_team_member    <- runDB (userIsProjectTeamMemberDB user_id project_id)
    (form, enctype) <- generateFormPost $
        projectNotificationsForm is_team_member mwiki_page mwiki_edit mblog_post
                                 mnew_pledge mupdated_pledge mdeleted_pledge
                                 mvolunteer_app
    defaultLayout $ do
        snowdriftDashTitle
            ("Notification Preferences for " <> projectName project)
            (userDisplayName $ Entity user_id user)
        $(widgetFile "project_notifications")

postProjectNotificationsR :: UserId -> ProjectId -> Handler Html
postProjectNotificationsR user_id project_id = do
    void $ checkEditUser user_id
    is_team_member <- runDB (userIsProjectTeamMemberDB user_id project_id)
    ((result, form), enctype) <- runFormPost $
        projectNotificationsForm is_team_member Nothing Nothing Nothing Nothing
                                 Nothing Nothing Nothing
    case result of
        FormSuccess notif_pref -> do
            forM_ (projectNotificationPref notif_pref) $ \(ntype, ndeliv) ->
                runDB $ updateProjectNotificationPrefDB
                    user_id project_id ntype ndeliv
            alertSuccess "Successfully updated the notification preferences."
            user <- runDB $ get404 user_id
            redirect $ UserR $ userNick user
        _ -> do
            project <- runYDB $ get404 project_id
            alertDanger "Failed to update the notification preferences."
            defaultLayout $(widgetFile "project_notifications")
