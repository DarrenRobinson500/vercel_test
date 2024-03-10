from django.contrib import admin
from django.urls import path, include
from app.views import *
from app.sudoku import *
from app.wordle import *
from django.conf import settings
from django.conf.urls.static import static
from django.views.generic.base import RedirectView

# favicon_view = RedirectView.as_view(url='/static/pinkyak.ico', permanent=True)

urlpatterns = [
    # Utilities
    path('admin/', admin.site.urls),
    path("", home, name="home"),
    path("home", home, name="home"),
    path('login/', login_user, name="login"),
    path('logout/', logout_user, name="logout"),
    # path("utility", utility, name="utility"),
    # path("utility_2", utility_2, name="utility_2"),
    path("load_data", load_data, name="load_data"),
    path("load_data_ind/<model_name>", load_data_ind, name="load_data_ind"),
    path('downloadexcel', downloadexcel, name="downloadexcel"),
    path('downloadpage', downloadpage, name="downloadpage"),

    # Dogs
    path("dog_new", dog_new, name="dog_new"),
    path("dogs", dogs, name="dogs"),
    path("dog/<id>", dog, name="dog"),
    path("dog_edit/<id>", dog_edit, name="dog_edit"),
    path("dog_diary", dog_diary, name="dog_diary"),
    path("dog_duration/<dur>", dog_duration, name="dog_duration"),
    path("booking/<id>", booking, name="booking"),
    path("booking_edit/<id>", booking_edit, name="booking_edit"),
    path("booking_delete/<id>", booking_delete, name="booking_delete"),

    # Notes
    path("notes", notes, name="notes"),
    path("note/<id>", note, name="note"),
    path("edit_note/<id>", edit_note, name="edit_note"),
    path("manual_rollforward/<id>", manual_rollforward, name="manual_rollforward"),
    path("up/<id>", up, name="up"),
    path("down/<id>", down, name="down"),
    path("delete_note/<id>", delete_note, name="delete_note"),
    # path("new_category", new_category, name="new_category"),

    # Misc
    path("diary", diary, name="diary"),
    path("diary_delete<id>", diary_delete, name="diary_delete"),
    path("events", events, name="events"),
    path("event_duration/<dur>", event_duration, name="event_duration"),
    path("quotes", quotes, name="quotes"),
    path("birthdays", birthdays, name="birthdays"),

    # Shopping
    path("shopping", shopping, name="shopping"),
    path("shopping_save", shopping_save, name="shopping_save"),
    path("shopping_edit/<id>", shopping_edit, name="shopping_edit"),
    path("shopping_delete/<id>", shopping_delete, name="shopping_delete"),
    path("shopping_up/<id>", shopping_up, name="shopping_up"),
    path("shopping_down/<id>", shopping_down, name="shopping_down"),
    path("shopping_clear", shopping_clear, name="shopping_clear"),

    # Games
    # path("clash", clash, name="clash"),
    # path("hero_inc/<id>/<hero>", hero_inc, name="hero_inc"),
    path('wordle', wordle, name='wordle'),
    path('wordle_remaining', wordle_remaining, name='wordle_remaining'),
    path('wordle_remaining/<id>', wordle_remaining, name='wordle_remaining'),
    path('wordle_remaining/<id>/<second_word>', wordle_remaining, name='wordle_remaining'),
    path('wordle/<entry>', wordle, name='wordle'),

    path('wordle_clear/<id>', wordle_clear, name='wordle_clear'),
    path('wordle_second/<word>', wordle_second, name='wordle_second'),
    path('add_wordle/<word>', add_wordle, name='add_wordle'),
    path('past_words', past_words, name='past_words'),
    path('word', word, name='word'),
    path('clear', clear, name='clear'),

    path('sudoku', sudoku, name='sudoku'),
    # path('timers', timers, name='timers'),
    # path('timer/<id>', timer, name='timer'),
    # path('tides', tides, name='tides'),
    # path('tennis', tennis, name='tennis'),
    # path('tennis_match_start', tennis_match_start, name='tennis_match_start'),
    # path('tennis_match/<id>', tennis_match, name='tennis_match'),
    # path('tennis_score/<id>/<a>/<b>', tennis_score, name='tennis_score'),
    # path('tennis_delete_game/<id>', tennis_delete_game, name='tennis_delete_game'),
    path("ckeditor5/", include("django_ckeditor_5.urls"), name="ck_editor_5_upload_file"),
]

urlpatterns += static(settings.MEDIA_URL, document_root=settings.MEDIA_ROOT)
urlpatterns += static(settings.STATIC_URL, document_root=settings.STATIC_ROOT)
