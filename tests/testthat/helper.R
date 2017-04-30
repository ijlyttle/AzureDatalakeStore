# some helper values to set up tests
#
#

helper <- list(
  tenant_id = Sys.getenv("azure_tenant_id"),
  subscription_id = Sys.getenv("azure_subscription_id"),
  resource_group_name = Sys.getenv("azure_resource_group_name"),
  account_test_name = Sys.getenv("azure_data_lake_store_account_test"),
  application_id = Sys.getenv("azure_native_application_id"),
  application_name = Sys.getenv("azure_native_application_name")
)

helper$names_status <- c(
  "location", "tags", "id", "name", "type",
  "properties.provisioningState", "properties.state",
  "properties.endpoint", "properties.accountId",
  "properties.creationTime", "properties.lastModifiedTime"
)

if (interactive()){

  helper$token <- AzureOAuth::oauth_token_azure(
    tenant_id = helper$tenant_id,
    application_id = helper$application_id,
    name = helper$application_name
  )

  helper$adls <- adls(
    base_url = adls_url(helper$account_test_name),
    token = helper$token
  )

}
