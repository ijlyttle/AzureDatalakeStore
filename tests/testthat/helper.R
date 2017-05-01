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

if (interactive()){

  helper$token <- AzureOAuth::oauth_token_azure(
    tenant_id = helper$tenant_id,
    application_id = helper$application_id,
    name = helper$application_name
  )

  # create if not created
  account_info <-
    AzureDatalakeStoreAccount::adla_list(
      token = helper$token,
      subscription_id = helper$subscription_id
    )

  # if account does not exist, create
  if (!(helper$account_test_name %in% account_info$name)){

    AzureDatalakeStoreAccount::adla_create(
      token = helper$token,
      subscription_id = helper$subscription_id,
      resource_group_name = helper$resource_group_name,
      name = helper$account_test_name,
      body = list(location = "centralus")
    )

    Sys.sleep(60)
  }

  helper$adls <- AzureDatalakeStore::adls(
    base_url = adls_url(helper$account_test_name),
    token = helper$token
  )

  is_empty <- is.null(adls_list_status(helper$adls))

  # stop if not empty
  assertthat::assert_that(
    is_empty,
    msg = paste("Datalake Store:", helper$account_test_name, "is not empty.")
  )

}
